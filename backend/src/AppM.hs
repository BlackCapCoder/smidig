{-# LANGUAGE UndecidableInstances #-}
module AppM
  ( module Control.Monad.Reader
  , module Database.Selda
  , module AppM
  )
  where

import Control.Monad.Reader
import Data.Aeson hiding (Result)
import Database.Selda
import Database.Selda.SQLite
import Servant
import Servant.Auth.Server


data Access = Public | Private

type family AppM (a :: Access) where
  AppM Public  = SeldaT Handler
  AppM Private = ReaderT User (AppM Public)


database = withSQLite "db.sqlite"


nt :: AppM Public a -> Handler a
nt = database

appWithContext
  :: forall api ctx
   . HasServer api ctx
  => Context ctx
  -> ServerT api (AppM Public)
  -> Application
appWithContext x = serveWithContext api x . hoistServerWithContext api ctx nt
  where api = Proxy @api
        ctx = Proxy @ctx

app :: forall api
       . HasServer api [CookieSettings, JWTSettings]
      => (CookieSettings -> JWTSettings -> ServerT api (AppM Public))
      -> IO Application
app server = do
  key <- generateKey
  let jwtCfg = let x = defaultJWTSettings key in x
      cs     = defaultCookieSettings {cookieXsrfSetting=Nothing}
      cfg    = cs :. jwtCfg :. EmptyContext
      api    = Proxy @api
      ctx    = Proxy :: Proxy '[CookieSettings, JWTSettings]

  pure $ serveWithContext api cfg
       $ hoistServerWithContext api ctx nt
       $ server cs jwtCfg



type UserID = ID User

data User = User
  { uid      :: UserID
  , username :: Text
  , password :: Text
  , age      :: Int
  , pic      :: Maybe Text
  } deriving ( Eq, Show, Generic, ToJSON, FromJSON
             , ToJWT, FromJWT, SqlRow
             )

users :: Table User
users = table "users" [#uid :- autoPrimary]

instance ToJSON (ID a) where
  toJSON = toJSON . fromId

instance FromJSON (ID a) where
  parseJSON = fmap toId . parseJSON

instance FromHttpApiData (ID a) where
  parseQueryParam = fmap toId . parseQueryParam

