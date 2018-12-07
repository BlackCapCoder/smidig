module Event where

import Utils
import User hiding (Api, server)


data Event = Event
  { title :: Text
  , owner :: User
  }
  deriving (Eq, Show, Generic)

instance ToJSON   Event
instance FromJSON Event


myEvents :: [Event]
myEvents
  = [ Event "Bowling night"   somebody
    , Event "Romantic dinner" somebody
    , Event "Opera training"  somebody
    ] where somebody = User "somepony" 5

----

type Api =
  "list" :> Get '[JSON] [Event]

server :: IO (Server Api)
server = pure $ pure myEvents

