module Login where

import Servant
import Servant.Auth.Server hiding (def)
import Servant.Auth.Server.Internal.ThrowAll
import Data.Aeson hiding (Result)

import Database.Selda
import Data.Maybe
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.State
import Debug.Trace
import Data.Time.Clock

import AppM


data Credentials = Credentials
  { username :: Text
  , password :: Text
  }
  deriving (ToJSON, FromJSON, Generic)


type Protected api = Auth '[Cookie, JWT] User :> api


-- Unlock a private API given that the user is authorized
unlock :: forall api. HasServer api '[Cookie]
       => ServerT            api  (AppM Private)
       -> ServerT (Protected api) (AppM Public)
unlock server auth
  = hoistServerWithContext
      (Proxy @api)
      (Proxy @'[Cookie])
      ( case auth of
          Authenticated user -> unPrivate user
          _ -> const . lift $
            throwAll err301 { errHeaders = [("Location", "/")] }
      )
      server


unPrivate :: User -> AppM Private a -> AppM Public a
unPrivate = runS
  where runR s m   = runReaderT m s
        runS usr m = fmap fst $ runStateT (updateLastSeen >> m) usr


updateLastSeen = do
  -- myid <- gets uid
  -- now  <- liftIO getCurrentTime
  -- update_ users (\u -> u ! #uid .== literal myid) $ flip with
  --   [ #lastSeen := literal now ]
  pure ()


type SetCookie' =
  Headers [ Header "Set-Cookie" SetCookie
          , Header "Set-Cookie" SetCookie ]
  NoContent

cookieAuth :: CookieSettings -> JWTSettings -> Credentials -> AppM Public SetCookie'
cookieAuth cs jwts cr = do
  user   <- checkCreds cr >>= maybe (lift $ throwError err401) pure
  cookie <- liftIO $ acceptLogin cs jwts user

  case cookie of
    Nothing    -> lift $ throwError err401
    Just apply -> pure $ apply NoContent

checkCreds :: Credentials -> AppM Public (Maybe User)
checkCreds Credentials {..} =
  fmap listToMaybe $ query do
    u <- select users
    restrict (u ! #username .== literal username)
    restrict (u ! #password .== literal password)
    pure u

type Login =
  "login"
     :> ReqBody '[JSON] Credentials
     :> PostNoContent '[JSON] SetCookie'

login :: CookieSettings -> JWTSettings -> ServerT Login (AppM Public)
login cs jwts cr = cookieAuth cs jwts cr


type Logout =
  "logout"
    :> GetNoContent '[JSON] SetCookie'

logout :: CookieSettings -> ServerT Logout (AppM Public)
logout cs = do
  pure . clearSession cs $ NoContent
  -- lift $ throwAll err301 { errHeaders = [("Location", "/")] }
