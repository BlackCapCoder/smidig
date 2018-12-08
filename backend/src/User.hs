module User where

import Utils


type UserID = ID User

data User = User
  { uid      :: UserID
  , username :: Text
  , age      :: Int
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

data LoginReq = LoginReq
  { username :: Text
  , password :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)


type Api = "user"  :> QueryParam "id" Int :> Get '[JSON] (Maybe User)
      :<|> "login" :> ReqBody '[JSON] LoginReq :> Post '[JSON] ()


users :: Table User
users = table "users" [#uid :- autoPrimary]

db = liftIO . withSQLite "users.sqlite"


server :: IO (Server Api)
server = do
  db $ tryCreateTable users
  pure $ queryUser :<|> login

  where queryUser (Just uid) = fmap listToMaybe . db $ query
          [ u | u <- select users
          , _ <- restrict (u ! #uid .== literal (toId uid)) ]

        login (LoginReq u p) = error "Not Implemented"


