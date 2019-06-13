module Group where

import Utils
import AppM
import Tags
import Chat (ChatID, chats, Chat (..))
import Notifications
import qualified Data.Set as S
import Data.Maybe
import GHC.Exts (groupWith)

#define KitchenSink Eq, Show, Generic, ToJSON, FromJSON
#define Row (KitchenSink, SqlRow)


type Location = Text

type GroupID = ID Group
data Group = Group
  { gid      :: GroupID
  , title    :: Text
  , subtitle :: Text
  , desc     :: Text
  , isPublic :: Bool
  , location :: Maybe Location
  , cid      :: ChatID
  } deriving Row

groups :: Table Group
groups = table "groups" [#gid :- autoPrimary]


data Privileges
  = Normal
  | Admin
  deriving (KitchenSink, SqlType, Bounded, Enum, Read, Ord)

data GroupMember = GroupMember
  { gid  :: GroupID
  , uid  :: UserID
  , priv :: Privileges
  } deriving Row

groupMembers :: Table GroupMember
groupMembers = table "groupMembers" []


data GroupTags = GroupTags
  { gid :: GroupID
  , tid :: TagID
  } deriving Row

grouptags :: Table GroupTags
grouptags = table "grouptags" []

type CommentID = ID Comments
data Comments  = Comments
  { cid     :: CommentID
  , gid     :: GroupID
  , owner   :: UserID
  , content :: Text
  } deriving Row

comments :: Table Comments
comments = table "groupComments" [#cid :- autoPrimary]



-------------------


data MkGroupReq = MkGroupReq
  { req_title :: Text
  , req_sub   :: Text
  , req_desc  :: Text
  , req_pub   :: Bool
  , req_loc   :: Maybe Location
  , req_tags  :: S.Set TagID
  } deriving (KitchenSink)


instance Backend Group where
  type Acc Group = Private
  type API Group
       = "group"         :> QueryParam "id" GroupID :> Get '[JSON] (Maybe Group)
    :<|> "joinGroup"     :> QueryParam "id" GroupID :> GetNoContent '[JSON] NoContent
    :<|> "leaveGroup"    :> QueryParam "id" GroupID :> GetNoContent '[JSON] NoContent
    :<|> "makeGroup"     :> MkGroupReq ~> GroupID
    :<|> "deleteGroup"   :> GroupID    ~> Bool
    :<|> "groupMembers"  :> QueryParam "id" GroupID :> Get '[JSON] [GroupMember]
    :<|> "inviteToGroup" :> (GroupID, UserID, Privileges) ~> NoContent
    :<|> "groups"        :> Maybe TagID ~> [Group]

    :<|> "addGroupComment" :> ReqBody '[JSON] (GroupID, Text) :> Post '[JSON] CommentID
    :<|> "groupComments"   :> ReqBody '[JSON] (GroupID) :> Post '[JSON] [Comments]

  server = getGroup
      :<|> joinGroup
      :<|> leaveGroup
      :<|> mkGroup
      :<|> rmGroup
      :<|> listGroupMembers
      :<|> inviteToGroup
      :<|> listGroups

      :<|> postComment
      :<|> getComments

postComment (eid, txt) = do
  ownerID <- getID
  insertWithPK comments
    [ Comments def eid ownerID txt ]

getComments = getByID' comments #gid

getGroup = getByIDM groups #gid

joinGroup id = do
  Just g  <- getGroup id
  True    <- pure $ Group.isPublic g
  myId    <- gets AppM.uid
  Nothing <- getGroupMember (fromJust id) myId
  insert_ groupMembers
    [ GroupMember (fromJust id) myId Normal ]
  pure NoContent

leaveGroup gid = do
  deleteFrom_ groupMembers . is #uid =<< getID
  pure NoContent

getGroupMember gid uid = fmap listToMaybe . query $
  groupMembers `having` and'
    [ #gid ?= gid
    , #uid ?= uid ]

listGroupMembers (Just gid) = query $
  groupMembers `having` #gid ?= gid


inviteToGroup (gid, uid, ps) = do
  Just g  <- getGroup (pure gid)
  False   <- pure $ Group.isPublic g
  Just m  <- getGroupMember gid =<< gets AppM.uid
  True    <- pure $ priv m > Normal
  Nothing <- getGroupMember gid uid
  insert_ groupMembers [ GroupMember gid uid ps ]
  notify uid GroupInvitation gid
  pure NoContent


mkGroup req@MkGroupReq {..} = do
  ownerID <- gets AppM.uid

  cid <- insertWithPK chats
    [Chat def req_pub]

  gid <- insertWithPK groups
    [Group def req_title req_sub req_desc req_pub req_loc cid]

  insert_ groupMembers
    [GroupMember gid ownerID Admin]

  ts <- fmap catMaybes . forM (S.toList req_tags) $ get1 tags #tid
  insert_ grouptags $ GroupTags gid <$> ts

  pure gid

rmGroup gid = do
  myId <- gets AppM.uid
  gs <- query do
    g <- #gid `from` groups `having` #gid ?= gid
    m <- select groupMembers

    restrict $ m ! #gid .== g
           .&& is #uid  myId  m
           .&& is #priv Admin m

    pure $ g

  forM_ gs $ \g -> do
    deleteFrom_ groups       $ #gid ?= g
    deleteFrom_ groupMembers $ #gid ?= g
    -- TODO: delete chat. Should be a function in chat module

  pure . not $ null gs

hasAccess :: Group -> AppM Private Bool
hasAccess g
  | Group.isPublic g = pure True
  | otherwise        = do
      myId <- gets AppM.uid
      fmap (not . null) . query $
         groupMembers `having` #uid ?= myId


groupsByTag :: AppM Private [(TagID, [Group])]
groupsByTag = do
  x <- query do
    t <- select grouptags
    g <- select groups
    restrict $ g ! #gid .== t ! #gid
    order (t ! #tid) ascending
    pure $ t ! #tid :*: g

  pure $ (\x -> (first $ head x, second <$> x)) <$> groupWith first x


listGroups :: Maybe TagID -> AppM Private [Group]
listGroups mt =
  filterM hasAccess
    =<< maybe (query $ select groups)
              (\t -> join . maybeToList . lookup t <$> groupsByTag)
              mt

