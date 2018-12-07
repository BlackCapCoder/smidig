module Backend
  ( app
  )
  where

import Servant
import qualified Event
import qualified User


type Api = Event.Api :<|> User.Api

app :: IO Application
app = do
  e <- Event.server
  u <- User.server
  pure . serve (Proxy :: Proxy Api) $
    e :<|> u

