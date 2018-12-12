module File where

import Utils
import Servant.Server.StaticFiles
import Network.Wai
import Control.Exception (bracket_)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)


type Api = Raw

folder :: FilePath
folder = "../frontend"


-- Serves files from `folder`
-- If html file, splice it into theme.html by replacing the % character
server :: IO (Server Api)
server = do
  (p1,p2) <- B.span (/=37) <$> B.readFile (folder ++ "/theme.html")

  let p1' = byteString p1
      p2' = byteString $ B.tail p2

  pure . (>>= pure . addTheme p1' p2') $ serveDirectoryWebApp folder


addTheme p1 p2 base req resp = base req $ resp . splice
  where splice r
          | B.isSuffixOf ".html" $ rawPathInfo req
          , (status, headers, f) <- responseToStream r
          = responseStream status headers $ \write flush -> do
              write p1
              f $ \sb -> sb write flush
              write p2
              flush
          | otherwise = r


