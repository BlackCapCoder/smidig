{-# LANGUAGE DuplicateRecordFields, RecordWildCards, MultiWayIf #-}
module User where

import Utils
import AppM


instance Backend User where
  type Acc User = Public -- Private
  type API User
      = "user"     :> QueryParam "id" UserID :> Get '[JSON] (Maybe User)
   :<|> "register" :> RegisterReq ~> RegisterResult

  server = do
    queryUser :<|> register

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

data RegisterReq = RegisterReq 
  { username :: Text
  , password :: Text
  , age      :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

type RegisterResult = Either Text UserID



data WhoAmI
instance Backend WhoAmI where
  type Acc WhoAmI = Private
  type API WhoAmI = "whoami" :> Get '[JSON] User

  server = ask

