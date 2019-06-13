{-# LANGUAGE DuplicateRecordFields, RecordWildCards, MultiWayIf #-}
module User where

import Utils
import AppM
import Event
import Login (SetCookie')
import Notifications
import Data.Time.Clock

#define KitchenSink Eq, Show, Generic, ToJSON, FromJSON
#define Row (KitchenSink, SqlRow)


data RegisterReq = RegisterReq
  { username :: Text
  , password :: Text
  , age      :: Int
  } deriving Row

type RegisterResult = Either Text UserID


type FavoriteID = ID Favorite

data Favorite = Favorite
  { fid :: FavoriteID
  , eid :: EventID
  , uid :: UserID
  } deriving Row

favorites :: Table Favorite
favorites = table "favorites" [#fid :- autoPrimary]


type FriendID = ID Friend

data Friend = Friend
  { user1    :: UserID
  , user2    :: UserID
  , accepted :: Bool    -- user2 is the friendee
  } deriving Row

friends :: Table Friend
friends = table "friends" []



data FriendshipStatus
  = Friends
  | NotFriends
  | PendingAccept Bool -- True if we can accept
  deriving (Generic, ToJSON)

data SetUserInfo = SetUserInfo
  { username :: Text
  , age      :: Int
  , desc     :: Text
  }
  deriving (Generic, FromJSON)

data ChangePasswordReq = ChangePasswordReq
  { oldPass :: Text
  , newPass :: Text
  }
  deriving (Generic, FromJSON)

data WhoAmI
instance Backend WhoAmI where
  type Acc WhoAmI = Private
  type API WhoAmI = "whoami" :> Get '[JSON] User

  server = get


instance Backend User where
  type Acc User = Public -- Private
  type API User
      = "user"     :> QueryParam "id" UserID :> Get '[JSON] (Maybe User)
   :<|> "register" :> RegisterReq ~> RegisterResult


  server = queryUser
      :<|> register

queryUser
  = getByIDM users #uid

register RegisterReq{..} = do
  us <- query $
    users `having` #username ?= username

  if | not $ null us
     -> pure $ Left "There is already a user with that name!"

     | otherwise
     -> do now <- liftIO getCurrentTime
           uid <- insertWithPK users
             [User def username password age Nothing "" now]
           return $ Right uid


data LoggedUser
instance Backend LoggedUser where
  type Acc LoggedUser = Private
  type API LoggedUser
       = "myfavorites" :> Get '[JSON] [Favorite]
    :<|> "addFavorite" :> (EventID) ~> FavoriteID
    :<|> "removeFavorite" :> (FavoriteID) ~> Bool
    :<|> "setUserInfo" :> ReqBody '[JSON] SetUserInfo :> Post '[JSON] Bool
    :<|> "myfriends"   :> Get '[JSON] [Friend]
    :<|> "befriend"    :> UserID ~> NoContent
    :<|> "listfriends" :> UserID ~> [UserID]
    :<|> "friendshipStatus" :> UserID ~> FriendshipStatus
    :<|> "changePassword"   :> ChangePasswordReq ~> Bool

  server = myfavorites
      :<|> addFavorite
      :<|> removeFavorite
      :<|> setUserInfo
      :<|> myFriends
      :<|> befriend
      :<|> listFriends
      :<|> friendshipStatus
      :<|> changePassword


changePassword :: ChangePasswordReq -> AppM Private Bool
changePassword ChangePasswordReq{..} = do
  myid <- getID
  ok   <- (== oldPass) . AppM.password . head <$> getByID' users #uid myid

  when ok . void . update users (#uid ?= myid)
          $ flip with [ #password := literal newPass ]

  pure ok

friendshipStatus :: UserID -> AppM Private FriendshipStatus
friendshipStatus uid = do
  fs <- filter (\f -> any (== uid) [user1 f, user2 f]) <$> myFriends

  pure if
    | null fs -> NotFriends
    | [f] <- fs -> if
      | accepted f -> Friends
      | otherwise  -> PendingAccept $ user1 f == uid


listFriends :: UserID -> AppM Private [UserID]
listFriends uid = f <$> query do
  friends `having` and'
    [ (! #accepted)
    , areFriends uid ]

  where
    f = fmap \f ->
          if user1 f == uid
            then user2 f
            else user1 f

areFriends uid = or' [ #user1 ?= uid, #user2 ?= uid ]

setUserInfo SetUserInfo{..} = do
  myid <- getID

  n <- update users (#uid ?= myid) $ flip with
    [ #username := literal username
    -- , #password := literal (AppM.password usr)
    , #age      := literal age
    -- , #pic      := literal (AppM.pic      usr)
    , #desc     := literal desc
    ]

  when (n >= 1) do
    modify \u -> (u :: User)
      { username = username
      , age      = age
      , desc     = desc
      }

  -- TODO: Set cookie
  pure $ n >= 1


myfavorites
  = query . having favorites . is #uid =<< getID

addFavorite eid = do
  myid <- getID

  -- Does the event exist?
  [_] <- query $ events `having` #eid ?= eid

  -- Already a favorite?
  [] <- query $ favorites `having` and' [#eid ?= eid, #uid ?= myid]

  insertWithPK favorites
    [ Favorite def eid myid ]

removeFavorite fid = do
  myid <- getID
  cnt  <- deleteFrom favorites $ and' [ #fid ?= fid, #uid ?= myid ]
  pure $ cnt > 0

myFriends = do
  myid <- getID
  query do
    friends `having` or'
      [ #user1 ?= myid
      , #user2 ?= myid ]

befriend uid = do
  myid <- getID

  -- You can't become friends with yourself
  False <- pure $ myid == uid

  -- Pending request?  TODO: upsert
  cnt <- update friends
        (and' [ #user2 ?= myid, #user1 ?= uid ])
        (flip with [ #accepted := true ])

  -- Send friend request
  when (cnt == 0) $ void do

    -- Already friends?
    [] <- query $ friends `having` and'
      [ #user1 ?= myid
      , #user2 ?= uid ]

    insert friends
      [ Friend myid uid False ]

    notify uid FriendReq myid

  -- Remove friend request
  when (cnt > 0) . deleteFrom_ notifications $ and'
    [ #kind ?= FriendReq
    , #uid  ?= myid
    , #key  ?= fromId uid
    ]

  pure NoContent

