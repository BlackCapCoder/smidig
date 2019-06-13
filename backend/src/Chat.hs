module Chat where

import Utils hiding (Set)
import AppM hiding (Set)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable

#define KitchenSink Eq, Show, Generic, ToJSON, FromJSON
#define Row (KitchenSink, SqlRow)


type ChatID = ID Chat

data Chat = Chat
  { cid      :: ChatID
  , isPublic :: Bool
  } deriving Row

chats :: Table Chat
chats = table "chats" [#cid :- autoPrimary]

-- cid is ambigious
chid = cid :: Chat -> ChatID


data Participants = Participants
  { cid :: ChatID
  , uid :: UserID
  } deriving Row

participants :: Table Participants
participants = table "chatparticipants" []


type ChatMessageID = ID ChatMessage

data ChatMessage = ChatMessage
  { mid     :: ChatMessageID
  , cid     :: ChatID
  , sender  :: UserID
  , content :: Text
  } deriving Row

chatMessages :: Table ChatMessage
chatMessages = table "chatmessages" [#mid :- autoPrimary]


instance Backend Chat where
  type Acc Chat = Private
  type API Chat
       = "myChats"       :> Get '[JSON] [Chat]
    :<|> "mkChat"        :> (Bool, Set UserID) ~> ChatID
    :<|> "putChat"       :> (ChatID, Text)     ~> ChatMessageID
    :<|> "readChat"      :> ChatID             ~> [ChatMessage]
    :<|> "readChatSince" :> (ChatID, ChatMessageID) ~> [ChatMessage]
    :<|> "chatWithUser"  :> (UserID) ~> ChatID
    :<|> "joinOpenChat"  :> (ChatID) ~> ChatID
    :<|> "chatParticipants" :> (ChatID) ~> [Participants]

  server = myChats
      :<|> mkChat
      :<|> putChat
      :<|> readChat
      :<|> readChatSince
      :<|> chatWithUser
      :<|> joinOpenChat
      :<|> chatParticipants

chatParticipants :: ChatID -> AppM Private [Participants]
chatParticipants = getByID' participants #cid

joinOpenChat :: ChatID -> AppM Private ChatID
joinOpenChat chat = do

  -- Does the chat exist and is open?
  _:_ <- query $ chats `having` and'
    [ (! #isPublic)
    , #cid ?= chat ]

  -- Already in chat?
  q <- filter ((== chat) . chid) <$> myChats

  when (null q) do
    myid <- getID
    insert_ participants
      [ Participants chat myid ]

  pure chat


-- TODO: Use aggregate, this is ugly!
chatWithUser :: UserID -> AppM Private ChatID
chatWithUser uid
  = maybe new old . listToMaybe =<< do
      myChats >>= filterM \(chid->cid) ->
        not . null <$> query do
          participants `having` and'
            [ #cid ?= cid
            , #uid ?= uid ]
  where
    old = pure . chid
    new = mkChat (False, S.fromList [uid])


readChatSince :: (ChatID, ChatMessageID) -> AppM Private [ChatMessage]
readChatSince (chat, since)
  = filter p <$> readChat chat where
      p msg = mid msg > since

readChat :: ChatID -> AppM Private [ChatMessage]
readChat
  = getByID' chatMessages #cid

putChat :: (ChatID, Text) -> AppM Private ChatMessageID
putChat (chat, msg) = do
  myID <- getID
  _:_  <- query do
    c <- chats        `having` #cid ?= chat
    p <- participants `having` #uid ?= myID
    restrict $ p ! #cid .== c ! #cid
    pure p

  insertWithPK chatMessages
    [ ChatMessage def chat myID msg ]

mkChat :: (Bool, Set UserID) -> AppM Private ChatID
mkChat (isPub, uids') = do
  myID <- getID
  let uids = S.delete myID uids'
  False <- pure $ null uids

  us <- query $ distinct do
    from #uid (select users) `suchThat` \u ->
      u `isIn` (literal <$> S.toList uids)

  False <- pure $ null us

  chat <- insertWithPK chats
    [Chat def isPub]

  insert_ participants . flip fmap (myID:us) $
    Participants chat

  pure chat


myChats :: AppM Private [Chat]
myChats = do
  myID <- getID
  query do
    p <- participants `having` #uid ?= myID
    c <- select chats
    restrict $ c ! #cid .== p ! #cid
    pure c
