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
    :<|> "mkevent"      :> ReqBody '[JSON] MkEventReq :> Post '[JSON] (Maybe EventID)
    :<|> "joinEvent"    :> ReqBody '[JSON] EventID    :> Post '[JSON] Text

  server = listEvents
      :<|> getEvent
      :<|> getParticipants
      :<|> getPictures
      :<|> mkEvent
      :<|> joinEvent

    where listEvents      = lift . query $ select events
          getEvent        = lift . getByIDM events #eid
          getParticipants = lift . getByID  participants  #eid
          getPictures     = lift . getByID  pictures      #eid
          mkEvent     MkEventReq {..} = do
            ownerID <- asks AppM.uid
            evid    <- lift $ fmap (join . listToMaybe) do
              insert_ events
                [ Event def ownerID req_title req_desc req_place req_date ]
              query $ aggregate
                [ max_ (e ! #eid) | e <- select events ]

            case evid of
              Nothing -> pure Nothing
              Just e -> do
                lift $ insert_ participants [Participants e ownerID]
                pure $ Just e

          joinEvent evid = do
            uid <- asks AppM.uid

            lift $ do
              x <- fmap listToMaybe $ query do
                p <- select participants
                restrict (p ! #eid .== literal evid)
                restrict (p ! #uid .== literal uid)
                return p
              case x of
                Just  _ -> pure ()
                Nothing -> insert_ participants [Participants evid uid]
            return "OK"
