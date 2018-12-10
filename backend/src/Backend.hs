module Backend
  ( app
  )
  where

import Servant
import Servant.JS
import Data.Text (Text)
import qualified Event
import qualified User
import qualified File



type GetJs = "api.js" :> Get '[PlainText, JSON] Text
getJs = jsForAPI (Proxy :: Proxy Api) jquery

type Api = Event.Api :<|> User.Api

type FullApi = Api :<|> GetJs :<|> File.Api

app :: IO Application
app = do
  e <- Event.server
  u <- User.server
  f <- File.server
  pure . serve (Proxy :: Proxy FullApi) $
    (e :<|> u) :<|> pure getJs :<|> f

