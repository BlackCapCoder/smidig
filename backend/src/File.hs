{-# LANGUAGE UndecidableInstances #-}
module File where

import Utils
import Servant.Server.StaticFiles
import Network.Wai
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Builder (byteString)
import GHC.TypeLits
import qualified Network.HTTP.Media as M
import Network.HTTP.Media ((//), (/:))

import Servant.JS
import Servant.JS.Internal

import AppM
import Login


data HTML

instance Accept HTML where
   contentType _ = "text" // "html" /: ("charset", "utf-8")

instance MimeRender HTML BL.ByteString where
  mimeRender = const id


-- Single file
data File (pth :: Symbol) (name :: Symbol) (acc :: Access)

instance (KnownSymbol pth, MonadIO (AppM acc)) => Backend (File pth n acc) where
  type Acc (File pth n acc) = acc
  type API (File pth n acc) = n :> Get '[HTML] BL.ByteString
  server = liftIO do
    (p1,p2) <- BL.span (/=37) <$> BL.readFile "../frontend/theme.html"
    c <- BL.readFile . symbolVal $ Proxy @pth
    pure $ p1 <> c <> BL.tail p2


data File' (pth :: Symbol) (acc :: Access)
instance (KnownSymbol pth, MonadIO (AppM acc)) => Backend (File' pth acc) where
  type Acc (File' pth acc) = acc
  type API (File' pth acc) = Get '[HTML] BL.ByteString
  server = liftIO . BL.readFile . symbolVal $ Proxy @pth


-- Serve static files from some folder
data Folder (pth :: Symbol) (acc :: Access)

instance KnownSymbol pth => Backend (Folder pth acc) where
  type Acc (Folder pth acc) = acc
  type API (Folder pth acc) = Raw
  server = serveDirectoryWebApp . symbolVal $ Proxy @pth


-- Wrap responses in a theme
data Themed b (pth :: Symbol)

instance ( KnownSymbol pth
         , Backend b
         , MonadIO (AppM (Acc b))
         , Raw ~ API b
         ) => Backend (Themed b pth) where
  type Acc (Themed b pth) = Acc b
  type API (Themed b pth) = Raw

  server = do
    (p1,p2) <- liftIO $ B.span (/=37) <$> B.readFile (symbolVal $ Proxy @pth)

    let p1' = byteString p1
        p2' = byteString $ B.tail p2

    addTheme p1' p2' <$> server @b



addTheme p1 p2 base req resp = base req $ resp . splice
  where splice r
          | B.isSuffixOf ".html" pth
          , pth `notElem` ["/login.html"]
          , (status, headers, f) <- responseToStream r
          = responseStream status headers $ \write flush -> do
              write p1
              f $ \sb -> sb write flush
              write p2
              flush
          | otherwise = r
        pth = rawPathInfo req

