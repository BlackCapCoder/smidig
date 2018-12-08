module Event where

import Utils
import User  (UserID)


type EventID = ID Event

data Event = Event
  { eid   :: EventID
  , owner :: UserID
  , title :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)


type Api =
  "list" :> Get '[JSON] [Event]

events :: Table Event
events = table "events" [#eid :- autoPrimary]

db = liftIO . withSQLite "events.sqlite"


server :: IO (Server Api)
server = do
  db $ tryCreateTable events
  pure $ listEvents

  where listEvents = db $ query $ select events

