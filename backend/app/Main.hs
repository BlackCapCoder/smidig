module Main where

import Servant
import Network.Wai
import Network.Wai.Handler.Warp
import System.IO

import Backend (app)


startServer :: Int -> IO Application -> IO ()
startServer port app = do
  let settings =
        setPort port $
        setBeforeMainLoop (hPutStrLn stderr ("listening on port " ++ show port)) $
        defaultSettings

  runSettings settings =<< app


main :: IO ()
main = startServer 3000 app

