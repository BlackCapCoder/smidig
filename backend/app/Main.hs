module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
import System.IO

import Backend (app)


startServer :: Int -> Application -> IO ()
startServer port app = do
  let settings = setPort port
               . setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port))
               $ defaultSettings

  let tls = tlsSettings "ssl/certificate.pem" "ssl/key.pem"

  -- runSettings settings app
  runTLS tls settings app


main :: IO ()
main = startServer 3000 =<< app
