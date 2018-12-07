module User where

import Utils
import Numeric.Natural
import Data.Map (Map)
import qualified Data.Map as M


data User = User
  { username :: Text
  , age      :: Natural
  }
  deriving (Eq, Show, Generic)

instance ToJSON   User
instance FromJSON User

type Api =
  "user" :> QueryParam "username" Text :> Get '[JSON] (Maybe User)

users :: Map Text User
users = M.fromList
  [ ("bob", User "bob" 22)
  , ("roy", User "roy" 25)
  ]

server :: IO (Server Api)
server = pure $ queryUser
  where queryUser = pure . (flip M.lookup users =<<)


