{-# LANGUAGE OverloadedLabels #-}
module User where

import Utils
import Control.Monad


data User = User
  { uid      :: ID User
  , username :: Text
  , age      :: Int
  }
  deriving (Eq, Show, Generic)

instance ToJSON   User
instance FromJSON User
instance SqlRow   User

type Api =
  "user" :> QueryParam "id" Int :> Get '[JSON] (Maybe User)

users :: Table User
users = table "users" [#uid :- autoPrimary]

db = liftIO . withSQLite "users.sqlite"


server :: IO (Server Api)
server = do
  db $ tryCreateTable users
  pure $ queryUser

  where queryUser (Just uid) = fmap listToMaybe . db $ query
          [ u | u <- select users
          , _ <- restrict (u ! #uid .== literal (toId uid)) ]

