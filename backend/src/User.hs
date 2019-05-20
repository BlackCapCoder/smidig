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
                  uid <- insertWithPK users [User def username password age Nothing]
                  return $ Right uid


data LoggedUser
instance Backend LoggedUser where
  type Acc LoggedUser = Private
  type API LoggedUser
       = "myfavorites" :> Get '[JSON] [Favorite]
    :<|> "addFavorite" :> (EventID) ~> FavoriteID

  server = myfavorites
      :<|> addFavorite


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


data WhoAmI
instance Backend WhoAmI where
  type Acc WhoAmI = Private
  type API WhoAmI = "whoami" :> Get '[JSON] User

  server = ask

