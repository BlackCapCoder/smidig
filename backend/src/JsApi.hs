{-# LANGUAGE AllowAmbiguousTypes, PolyKinds, UndecidableInstances #-}
module JsApi where

import Servant.JS
import Servant.JS.Internal
import GHC.TypeLits (Symbol)

import Utils
import AppM


data JsFor (uri :: Symbol) a

instance ( Backend a
         , api ~ API a
         , Applicative (AppM (Acc a))
         , HasForeign NoTypes NoContent api
         , GenerateList NoContent (Foreign NoContent api)
         ) => Backend (JsFor uri a) where
  type API (JsFor uri a) = uri :> Get '[PlainText, JSON] Text
  type Acc (JsFor uri a) = Public

  server = pure $ jsForAPI (Proxy @api) jquery


type family Js uri l where
  Js uri '[x]      = x :<|> JsFor uri x
  Js uri (x ': xs) = x :<|> JsFor uri x :<|> Js uri xs



data JsApi (uri :: Symbol) (acc :: Access) api

instance ( HasForeign NoTypes NoContent api
         , GenerateList NoContent (Foreign NoContent api)
         , Applicative (AppM acc)
         ) => Backend (JsApi uri acc api) where
  type API (JsApi uri acc api) = uri :> Get [PlainText, JSON] Text
  type Acc (JsApi uri acc api) = acc

  server = pure $ jsForAPI (Proxy @api) vanillaJS

