{-# LANGUAGE DuplicateRecordFields, RecordWildCards, MultiWayIf #-}
module User where

import Utils
import AppM

data UserB

instance Backend UserB where
  type Acc UserB = Public -- Private
  type API UserB
      = "user"     :> QueryParam "id" UserID :> Get '[JSON] (Maybe User)
   :<|> "register" :> ReqBody '[JSON] RegisterReq :> Post '[JSON] RegisterResult

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

