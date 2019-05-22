{-# LANGUAGE DuplicateRecordFields, RecordWildCards, MultiWayIf #-}
module User where

import Utils
import AppM
import Event


instance Backend User where
  type Acc User = Public -- Private
  type API User
      = "user"     :> QueryParam "id" UserID :> Get '[JSON] (Maybe User)
   :<|> "register" :> RegisterReq ~> RegisterResult


  server = queryUser
      :<|> register

    where queryUser = getByIDM users #uid
          register RegisterReq{..} = do
            us <- query do
              u <- #username `from` select users
              restrict (u .== literal username)
              pure u

            if | not $ null us -> pure $ Left "There is already a user with that name!"
               | otherwise -> do
                  uid <- insertWithPK users
                    [User def username password age Nothing ""]
                  return $ Right uid


data LoggedUser
instance Backend LoggedUser where
  type Acc LoggedUser = Private
  type API LoggedUser
       = "myfavorites" :> Get '[JSON] [Favorite]
    :<|> "addFavorite" :> (EventID) ~> FavoriteID
    :<|> "setUserInfo" :> User ~> NoContent
    :<|> "myfriends"   :> Get '[JSON] [Friend]
    :<|> "befriend"    :> UserID ~> NoContent
    :<|> "listfriends" :> UserID ~> [UserID]
    :<|> "friendshipStatus" :> UserID ~> FriendshipStatus

  server = myfavorites
      :<|> addFavorite
      :<|> setUserInfo
      :<|> myFriends
      :<|> befriend
      :<|> listFriends
      :<|> friendshipStatus


friendshipStatus :: UserID -> AppM Private FriendshipStatus
friendshipStatus uid = do
  fs <- filter (\f -> user1 f == uid || user2 f == uid) <$> myFriends

  pure if
    | null fs -> NotFriends
    | [f] <- fs -> if
      | accepted f -> Friends
      | otherwise  -> PendingAccept $ user1 f == uid


listFriends :: UserID -> AppM Private [UserID]
listFriends uid = do
  fs <- query do
    f <- select friends
    restrict $ f ! #user1 .== literal uid
           .|| f ! #user2 .== literal uid
    pure f
  pure $ flip fmap fs \f ->
    if user1 f == uid
       then user2 f
       else user1 f

setUserInfo usr = do
  myid <- asks AppM.uid
  True <- pure $ AppM.uid usr == myid
  1 <- update users (\u -> u ! #uid .== literal myid) $
    flip with
      [ #username := literal (AppM.username usr)
      , #password := literal (AppM.password usr)
      , #age      := literal (AppM.age      usr)
      , #pic      := literal (AppM.pic      usr)
      , #desc     := literal (AppM.desc     usr)
      ]

  pure NoContent

myfavorites = do
  myid <- asks AppM.uid
  query do
    f <- select favorites
    restrict $ f ! #uid .== literal myid
    pure f

addFavorite eid = do
  myid <- asks AppM.uid

  -- Does the event exist?
  [_] <- query do
    ev <- select events
    restrict $ ev ! #eid .== literal eid
    pure ev

  -- Already a favorite?
  [] <- query do
    f <- select favorites
    restrict $ f ! #eid .== literal eid
    pure f

  insertWithPK favorites
    [ Favorite def eid myid ]

myFriends = do
  myid <- asks AppM.uid
  query do
    f <- select friends
    restrict $ f ! #user1 .== literal myid
           .|| f ! #user2 .== literal myid
    pure f

befriend uid = do
  myid <- asks AppM.uid

  -- You can't become friends with yourself
  False <- pure $ myid == uid

  -- Pending request?
  -- TODO: upsert
  cnt <- update friends
        (\f -> (f ! #user1 .== literal myid
            .|| f ! #user2 .== literal myid)
            .&&(f ! #user1 .== literal uid
            .|| f ! #user2 .== literal uid)
            .&&(f ! #accepted .== literal False)
        ) $ flip with
        [ #accepted := literal True ]

  when (cnt == 0) $ void do

    -- Already friends?
    [] <- query do
      f <- select friends
      restrict $ f ! #user1 .== literal myid
            .|| f ! #user2 .== literal myid
      restrict $ f ! #user1 .== literal uid
            .|| f ! #user2 .== literal uid
      pure f

    insert friends
      [ Friend myid uid False ]

    -- TODO: Send notification to friendee

  liftIO $ putStrLn "befriended!"

  pure NoContent


data RegisterReq = RegisterReq 
  { username :: Text
  , password :: Text
  , age      :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

type RegisterResult = Either Text UserID


type FavoriteID = ID Favorite

data Favorite = Favorite
  { fid :: FavoriteID
  , eid :: EventID
  , uid :: UserID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

favorites :: Table Favorite
favorites = table "favorites" [#fid :- autoPrimary]


type FriendID = ID Friend

data Friend = Friend
  { user1    :: UserID
  , user2    :: UserID
  , accepted :: Bool    -- user2 is the friendee
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

friends :: Table Friend
friends = table "friends" []

data FriendshipStatus
  = Friends
  | NotFriends
  | PendingAccept Bool -- True if we can accept
  deriving (Generic, ToJSON)


data WhoAmI
instance Backend WhoAmI where
  type Acc WhoAmI = Private
  type API WhoAmI = "whoami" :> Get '[JSON] User

  server = ask

