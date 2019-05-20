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
  , lop :: LevelOfParticipation
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

data LevelOfParticipation
  = Interested | Going
  deriving ( Eq, Show, Generic, ToJSON, FromJSON, SqlType
           , Bounded, Enum, Read
           )

data Pictures = Pictures
  { eid  :: EventID
  , pth  :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

type CommentID = ID Comments

data Comments = Comments
  { cid     :: CommentID
  , eid     :: EventID
  , owner   :: UserID
  , content :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

comments :: Table Comments
comments = table "comments" [#cid :- autoPrimary]

events :: Table Event
events = table "events" [#eid :- autoPrimary]

participants :: Table Participants
participants = table "participants" []

pictures :: Table Pictures
pictures = table "pictures" []


instance Backend Event where
  type Acc Event = Private
  type API Event
      =  "listevents"   :> Get '[JSON] [Event]
    :<|> "getevent"        :> QueryParam "id" EventID    :> Get  '[JSON] (Maybe Event)
    :<|> "participants" :> QueryParam "id" EventID    :> Get  '[JSON] [Participants]
    :<|> "pictures"     :> QueryParam "id" EventID    :> Get  '[JSON] [Pictures]
    :<|> "mkevent"      :> ReqBody '[JSON] MkEventReq :> Post '[JSON] EventID
    :<|> "joinEvent"    :> ReqBody '[JSON] (EventID, LevelOfParticipation)    :> Post '[JSON] NoContent
    :<|> "addComment" :> ReqBody '[JSON] (EventID, Text) :> Post '[JSON] CommentID
    :<|> "eventcomments" :> ReqBody '[JSON] (EventID) :> Post '[JSON] [Comments]

  server = listEvents
      :<|> getEvent
      :<|> getParticipants
      :<|> getPictures
      :<|> mkEvent
      :<|> joinEvent
      :<|> postComment
      :<|> getcomments

    where listEvents      = query $ select events
          getEvent        = getByIDM events        #eid
          getParticipants = getByID  participants  #eid
          getPictures     = getByID  pictures      #eid
          mkEvent MkEventReq {..} = do
            ownerID <- asks AppM.uid
            evid    <- insertWithPK events
              [Event def ownerID req_title req_desc req_place req_date]
            liftIO $ print evid

            insert_ participants
              [Participants evid ownerID Going]

            pure evid

          joinEvent (evid, lop) = do
            uid <- asks AppM.uid

            -- Does the event exist?
            Just _ <- get1 events #eid evid

            -- Already a participant?
            [] <- query do
              p <- select participants
              restrict (p ! #eid .== literal evid)
              restrict (p ! #uid .== literal uid)
              pure p

            insert_ participants [Participants evid uid lop]

            pure NoContent

          postComment (eid, txt) = do
            ownerID <- asks AppM.uid
            cid <- insertWithPK comments
              [ Comments def eid ownerID txt ]
            pure cid

          getcomments (eid) = query do
            e <- select comments
            restrict $ e ! #eid .== literal eid
            pure e



get1 table sel needle = listToMaybe <$> query do
  x <- sel `from` select table
  restrict $ x .== literal needle
  pure x
