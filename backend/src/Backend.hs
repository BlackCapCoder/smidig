module Backend
  ( app
  )
  where

import Servant
import Servant.JS
import Data.Text (Text)
import qualified Event
import qualified User


type GetJs = "api.js" :> Get '[PlainText, JSON] Text
getJs = jsForAPI (Proxy :: Proxy Api) jquery

type Api = Event.Api :<|> User.Api
      :<|> GetJs


app :: IO Application
app = do
  e <- Event.server
  u <- User.server
  pure . serve (Proxy :: Proxy Api) $
    e :<|> u :<|> pure getJs

