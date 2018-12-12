module File where

import Utils
import Servant.Server.StaticFiles
import Network.Wai
import Control.Exception (bracket_)
import qualified Data.ByteString as B
import Data.ByteString.Builder (byteString)


type Api = Raw


server :: IO (Server Api)
server = do
  (p1,p2) <- B.span (/=37) <$> B.readFile "../frontend/theme.html"
  pure . (>>= pure . addTheme (byteString p1) (byteString $ B.tail p2))
    $ serveDirectoryWebApp "../frontend"

addTheme p1 p2 base req resp = base req (resp . trans)
  where trans r
          | B.isSuffixOf ".html" $ rawPathInfo req
          , (status, headers, f) <- responseToStream r
          = responseStream status headers $ \write flush -> do
              write p1
              f $ \sb -> sb write flush
              write p2
          | otherwise = r


