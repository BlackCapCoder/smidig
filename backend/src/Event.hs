module Event where

import Utils
import User  (UserID)


type EventID = ID Event

data Event = Event
  { eid   :: EventID
  , owner :: UserID
  , title :: Text
  , desc  :: Text
  , place :: Text
  , date  :: UTCTime
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)


type Api =
  "events" :> Get '[JSON] [Event]

events :: Table Event
events = table "events" [#eid :- autoPrimary]

db = liftIO . withSQLite "events.sqlite"


server :: IO (Server Api)
server = do
  db $ do
    tryCreateTable events
    -- insert_ events
    --   [ Event def (toId 1) "Bowling night"   "Win a beer!" "Oslo" def
    --   , Event def (toId 2) "Romantic dinner" "Like a regular dinner, but with tea lights" "Drammen" def
    --   ]

  pure $ listEvents

  where listEvents = db $ query $ select events

