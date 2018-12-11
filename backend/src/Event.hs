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

data MkEventReq = MkEventReq
  { req_title :: Text
  , req_desc  :: Text
  , req_place :: Text
  , req_date  :: UTCTime
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)


data Participants = Participants
  { eid :: EventID
  , uid :: UserID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

data Pictures = Pictures
  { eid  :: EventID
  , pth  :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

type Api = "events" :> Get '[JSON] [Event]
      :<|> "event"  :> QueryParam "id" EventID :> Get '[JSON] (Maybe Event)
      :<|> "participants" :> QueryParam "id" EventID :> Get '[JSON] [Participants]
      :<|> "pictures" :> QueryParam "id" EventID :> Get '[JSON] [Pictures]
      :<|> "mkevent" :> ReqBody '[JSON] MkEventReq :> Post '[JSON] (Maybe EventID)
      :<|> "joinEvent" :> ReqBody '[JSON] EventID :> Post '[JSON] Text

events :: Table Event
events = table "events" [#eid :- autoPrimary]

participants :: Table Participants
participants = table "participants" []

pictures :: Table Pictures
pictures = table "pictures" []

db = liftIO . withSQLite "events.sqlite"


server :: IO (Server Api)
server = do
  db $ do
    tryCreateTable events
    tryCreateTable participants
    tryCreateTable pictures

  pure $ listEvents :<|> getEvent :<|> getParticipants :<|> getPictures
                    :<|> mkEvent :<|> joinEvent

  where listEvents      = db . query $ select events
        getEvent        = getByIDM db events       #eid
        getParticipants = getByID  db participants #eid
        getPictures     = getByID  db pictures     #eid
        mkEvent     req = do
          let ownerID = toId 1

          evid <- fmap (join . listToMaybe) . db $ do
            insert_ events
              [Event def ownerID (req_title req) (req_desc req) (req_place req) (req_date req)]
            query $ aggregate
              [ max_ (e ! #eid) | e <- select events ]

          case evid of
            Nothing -> pure Nothing
            Just e -> do
              db (insert_ participants [Participants e ownerID])
              pure $ Just e

        joinEvent evid = do
          db $ do
            let uid = toId 1
            x <- fmap listToMaybe . query $ do
              p <- select participants
              restrict (p ! #eid .== literal evid)
              restrict (p ! #uid .== literal uid)
              return p
            case x of
              Just _ -> pure ()
              Nothing -> insert_ participants [Participants evid uid]
          return "OK"
