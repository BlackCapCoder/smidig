module File where

import Utils
import Servant.Utils.StaticFiles


type Api = Raw


server :: IO (Server Api)
server = pure $ serveDirectory "../frontend"

