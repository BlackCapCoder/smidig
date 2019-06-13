{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# LANGUAGE AllowAmbiguousTypes, PolyKinds, UndecidableInstances #-}
module Utils
  ( module Utils
  , module Servant
  , module Database.Selda
  , module Database.Selda.SQLite
  , module Data.Aeson
  , module Control.Monad
  , module Data.Maybe
  , module Data.Function
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
import Data.Function
import AppM
import Login



type family Flatten f (a :: [k]) :: k where
  Flatten _ '[x]      = x
  Flatten f (x ': xs) = x `f` Flatten f xs

type Concat xs = Flatten (:<|>) xs

type a ~> b = ReqBody '[JSON] a :> Post '[JSON] b


(?=) = is
and' = foldl (liftM2 (.&&)) (const true)
or'  = foldl (liftM2 (.||)) (const false)

maybeM = maybe $ pure mzero


infixr 7 `having`
having t = suchThat $ select t


getByKey t sel
  = having t . is sel

getByID t
  = maybeM . getByID' t

getByIDM t sel
  = maybeM $ fmap listToMaybe . getByID' t sel

getByID' t sel
  = query . getByKey t sel

get1 tb sel
  = fmap listToMaybe
  . query
  . from sel
  . getByKey tb sel


-- Should be safe..?
-- Used for the Raw endpoint in File.hs
instance MonadIO (Tagged t) where
  liftIO = Tagged . unsafePerformIO



--------------

-- I really ought to make AppM a newtype..


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

  server = a :<|> b
    where a = authServer @(Acc a) @(API a) $ server @a
          b = authServer @(Acc b) @(API b) $ server @b

toApp :: forall b. ( Backend b
                   , AuthServer (Acc b) (API b)
                   , HasServer (API' b) '[CookieSettings, JWTSettings]
                   ) => IO Application
toApp = app @(API' b :<|> Login :<|> Logout :<|> Raw) s'
  where s  = authServer @(Acc b) @(API b) (server @b)
        s' a b = s
            :<|> login  a b
            :<|> logout a
            :<|> serveDirectoryWebApp "../frontend"

