{-# LANGUAGE AllowAmbiguousTypes, PolyKinds, UndecidableInstances #-}
module JsApi where

import Servant.JS
import Servant.JS.Internal
import GHC.TypeLits (Symbol)

import Utils
import AppM


data JsApi (uri :: Symbol) (acc :: Access) api

instance ( HasForeign NoTypes NoContent api
         , GenerateList NoContent (Foreign NoContent api)
         , Applicative (AppM acc)
         ) => Backend (JsApi uri acc api) where
  type API (JsApi uri acc api) = uri :> Get [PlainText, JSON] Text
  type Acc (JsApi uri acc api) = acc

  server = pure $ jsForAPI (Proxy @api) jquery

