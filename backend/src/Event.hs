module Event where

import Utils
import User  (UserID)


type EventID        = ID Event
type ParticipantsID = ID Participants
type PicturesID     = ID Pictures

data Event = Event
  { eid   :: EventID
  , owner :: UserID
  , title :: Text
  , desc  :: Text
  , place :: Text
  , date  :: UTCTime
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

data Participants = Participants
  { pid :: ParticipantsID
  , uid :: UserID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

data Pictures = Pictures
  { evid  :: EventID
  , pth   :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

type Api = "events" :> Get '[JSON] [Event]
      :<|> "event"  :> QueryParam "id" EventID :> Get '[JSON] (Maybe Event)
      :<|> "participants" :> QueryParam "id" ParticipantsID :> Get '[JSON] [Participants]
      :<|> "pictures" :> QueryParam "id" EventID :> Get '[JSON] [Pictures]

events :: Table Event
events = table "events" [#eid :- autoPrimary]

participants :: Table Participants
participants = table "participants" [#pid :- autoPrimary]

pictures :: Table Pictures
pictures = table "pictures" []

db = liftIO . withSQLite "events.sqlite"


server :: IO (Server Api)
server = do
  db $ do
    tryCreateTable events
    tryCreateTable participants

  pure $ listEvents :<|> getEvent :<|> getParticipants :<|> getPictures

  where listEvents      = db . query $ select events
        getEvent        = getByIDM db events       #eid
        getParticipants = getByID  db participants #pid
        getPictures     = getByID  db pictures     #evid


