{-# LANGUAGE UndecidableInstances #-}
module AppM
  ( module Control.Monad.Reader
  , module Control.Monad.State
  , module Database.Selda
  , module AppM
  )
  where

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Fail
import Control.Monad.Catch hiding (Handler)
import Data.Aeson hiding (Result)
import Database.Selda
import Database.Selda.SQLite
import Database.Selda.Backend
import Servant
import Servant.Auth.Server


data Access = Public | Private

type family AppM (a :: Access) where
  AppM Public  = SeldaT Handler
  AppM Private = StateT User (AppM Public)


-- data family AppM' (a :: Access) :: * -> *
--
-- newtype instance AppM' Public v = Pub (SeldaT Handler v)
--   deriving ( Functor
--            , Applicative
--            , Monad
--            , MonadIO
--            , MonadSelda
--            , MonadThrow
--            , MonadCatch
--            , MonadMask
--            , MonadFail
--            ) via (SeldaT Handler)
--
-- newtype instance AppM' Private v = Pri (ReaderT User (AppM' Public) v)
--   deriving ( Functor
--            , Applicative
--            , Monad
--            , MonadIO
--            , MonadReader User
--            , MonadThrow
--            , MonadCatch
--            , MonadMask
--            , MonadFail
--            ) via (ReaderT User (AppM' Public))
--
--
-- nt' :: AppM' a v -> v
-- nt' = undefined




instance MonadSelda (StateT User (SeldaT Handler)) where
  seldaConnection = lift seldaConnection

instance MonadFail (SeldaT Handler) where
  fail _ = lift $ throwError err501


database = withSQLite "db.sqlite"


nt = database :: AppM Public a -> Handler a

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


instance ToJSON (ID a) where
  toJSON = toJSON . fromId

instance FromJSON (ID a) where
  parseJSON = fmap toId . parseJSON

instance FromHttpApiData (ID a) where
  parseQueryParam = fmap toId . parseQueryParam



type UserID = ID User

data User = User
  { uid      :: UserID
  , username :: Text
  , password :: Text
  , age      :: Int
  , pic      :: Maybe Text
  , desc     :: Text
  , lastSeen :: UTCTime
  } deriving ( Eq, Show, Generic, ToJSON, FromJSON
             , ToJWT, FromJWT, SqlRow
             )

users :: Table User
users = table "users" [#uid :- autoPrimary]

getID = gets uid
