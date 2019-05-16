{-# LANGUAGE AllowAmbiguousTypes, PolyKinds, UndecidableInstances #-}
module Utils
  ( module Utils
  , module Servant
  , module Database.Selda
  , module Database.Selda.SQLite
  , module Data.Aeson
  , module Control.Monad
  , module Data.Maybe
  ) where

import Servant
import Database.Selda
import Database.Selda.SQLite
import Data.Aeson hiding (Result)
import Servant.Auth.Server
import Control.Monad.Trans
import System.IO.Unsafe
import Servant.Server.StaticFiles

import Control.Monad
import Data.Maybe
import AppM
import Login


type family Flatten f (a :: [k]) :: k where
  Flatten _ '[x]      = x
  Flatten f (x ': xs) = x `f` Flatten f xs


getByID' t sel id = query do
  x <- select t
  restrict (x ! sel .== literal id)
  return x

getByID t sel (Just id) = query do
  x <- select t
  restrict (x ! sel .== literal id)
  return x


getByIDM t sel = fmap listToMaybe . getByID t sel



taggedLiftIO :: MonadIO m => IO a -> Tagged t (m a)
taggedLiftIO = Tagged . liftIO

instance MonadIO (Tagged t) where
  liftIO = Tagged . unsafePerformIO


type family FixAPI acc api where
  FixAPI Public  a = a
  FixAPI Private a = Protected a

class AuthServer (acc :: Access) api where
  authServer :: ServerT api (AppM acc) -> ServerT (FixAPI acc api) (AppM Public)

instance AuthServer Public a where
  authServer = id

instance HasServer a '[Cookie] => AuthServer Private a where
  authServer = unlock @a

type API' a = FixAPI (Acc a) (API a)

class Backend a where
  type API a :: *
  type Acc a :: Access
  server :: ServerT (API a) (AppM (Acc a))


instance ( Backend a, Backend b
         , AuthServer (Acc a) (API a)
         , AuthServer (Acc b) (API b)
         ) => Backend (a :<|> b) where
  type API (a :<|> b) = API' a :<|> API' b
  type Acc (a :<|> b) = Public
  server = a' :<|> b'
    where a = server @a
          b = server @b
          a' = authServer @(Acc a) @(API a) a
          b' = authServer @(Acc b) @(API b) b

toApp :: forall b. ( Backend b
                   , AuthServer (Acc b) (API b)
                   , HasServer (API' b) '[CookieSettings, JWTSettings]
                   ) => IO Application
toApp = app @(API' b :<|> Login :<|> Raw) s'
  where s  = authServer @(Acc b) @(API b) (server @b)
        s' a b = s
            :<|> login a b
            :<|> serveDirectoryWebApp "../frontend"

