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


type Api = "events" :> Get '[JSON] [Event]
      :<|> "event"  :> QueryParam "id" EventID :> Get '[JSON] (Maybe Event)

events :: Table Event
events = table "events" [#eid :- autoPrimary]

db = liftIO . withSQLite "events.sqlite"


server :: IO (Server Api)
server = do
  db $ do
    tryCreateTable events

  pure $ listEvents :<|> getEvent

  where listEvents = db $ query $ select events

        getEvent (Just eid) = fmap listToMaybe . db . query $ do
          e <- select events
          restrict (e ! #eid .== literal eid)
          return e

