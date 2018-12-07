module Event where

import Utils
import User hiding (Api, server)


data Event = Event
  { title :: Text
  -- , owner :: User
  }
  deriving (Eq, Show, Generic)

instance ToJSON   Event
instance FromJSON Event


myEvents :: [Event]
myEvents
  = [ Event "Bowling night"  
    , Event "Romantic dinner"
    , Event "Opera training" 
    ]

----

type Api =
  "list" :> Get '[JSON] [Event]

server :: IO (Server Api)
server = pure $ pure myEvents

