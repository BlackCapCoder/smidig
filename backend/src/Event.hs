module Event where

import Utils
import AppM


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

events :: Table Event
events = table "events" [#eid :- autoPrimary]

participants :: Table Participants
participants = table "participants" []

pictures :: Table Pictures
pictures = table "pictures" []


data EventB

instance Backend EventB where
  type Acc EventB = Private
  type API EventB
      =  "events"       :> Get '[JSON] [Event]
    :<|> "event"        :> QueryParam "id" EventID    :> Get  '[JSON] (Maybe Event)
    :<|> "participants" :> QueryParam "id" EventID    :> Get  '[JSON] [Participants]
    :<|> "pictures"     :> QueryParam "id" EventID    :> Get  '[JSON] [Pictures]
    :<|> "mkevent"      :> ReqBody '[JSON] MkEventReq :> Post '[JSON] EventID
    :<|> "joinEvent"    :> ReqBody '[JSON] EventID    :> Post '[JSON] NoContent

  server = listEvents
      :<|> getEvent
      :<|> getParticipants
      :<|> getPictures
      :<|> mkEvent
      :<|> joinEvent

    where listEvents      = query $ select events
          getEvent        = getByIDM events        #eid
          getParticipants = getByID  participants  #eid
          getPictures     = getByID  pictures      #eid
          mkEvent MkEventReq {..} = do
            ownerID <- asks AppM.uid
            evid    <- insertWithPK events
              [Event def ownerID req_title req_desc req_place req_date]

            insert_ participants
              [Participants evid ownerID]

            pure evid

          joinEvent evid = do
            uid <- asks AppM.uid

            -- Does the event exist?
            Just _ <- get1 events #eid evid

            -- Already a participant?
            [] <- query do
              p <- select participants
              restrict (p ! #eid .== literal evid)
              restrict (p ! #uid .== literal uid)
              pure p

            insert_ participants [Participants evid uid]

            pure NoContent


get1 table sel needle = listToMaybe <$> query do
  x <- sel `from` select table
  restrict $ x .== literal needle
  pure x
