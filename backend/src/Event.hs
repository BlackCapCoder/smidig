module Event where

import Utils
import AppM
import Tags
import qualified Data.Text as T
import Data.Char
import qualified Data.Set as S
import Data.Maybe
import Chat (ChatID, chats, Chat (..))


type EventID = ID Event
data Event   = Event
  { eid   :: EventID
  , owner :: UserID
  , title :: Text
  , desc  :: Text
  , place :: Text
  , date  :: UTCTime
  , cid   :: ChatID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

events :: Table Event
events = table "events" [#eid :- autoPrimary]


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

participants :: Table Participants
participants = table "participants" []


data Pictures = Pictures
  { eid  :: EventID
  , pth  :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

pictures :: Table Pictures
pictures = table "pictures" []


type CommentID = ID Comments
data Comments  = Comments
  { cid     :: CommentID
  , eid     :: EventID
  , owner   :: UserID
  , content :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

comments :: Table Comments
comments = table "comments" [#cid :- autoPrimary]


data EventTags = EventTags
  { eid :: EventID
  , tid :: TagID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)


eventtags :: Table EventTags
eventtags = table "eventtags" []


-------------------------


data MkEventReq = MkEventReq
  { req_title :: Text
  , req_desc  :: Text
  , req_place :: Text
  , req_date  :: UTCTime
  , req_tags  :: S.Set TagID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

type TagSearch = Maybe TagID


instance Backend Event where
  type Acc Event = Private
  type API Event
      =  "listevents"    :> Get '[JSON] [Event]
    :<|> "listTags"      :> Get '[JSON] [Tags]
    :<|> "getevent"      :> QueryParam "id" EventID    :> Get  '[JSON] (Maybe Event)
    :<|> "participants"  :> QueryParam "id" EventID    :> Get  '[JSON] [Participants]
    :<|> "pictures"      :> QueryParam "id" EventID    :> Get  '[JSON] [Pictures]
    :<|> "mkevent"       :> ReqBody '[JSON] MkEventReq :> Post '[JSON] EventID
    :<|> "rmevent"       :> EventID ~> Bool
    :<|> "joinEvent"     :> ReqBody '[JSON] (EventID, LevelOfParticipation) :> Post '[JSON] NoContent
    :<|> "addComment"    :> ReqBody '[JSON] (EventID, Text) :> Post '[JSON] CommentID
    :<|> "eventcomments" :> ReqBody '[JSON] (EventID) :> Post '[JSON] [Comments]
    :<|> "searchevents"  :> ReqBody '[JSON] (Text, TagSearch) :> Post '[JSON] [Event]
    :<|> "eventTags"     :> EventID ~> [Tags]

  server = listEvents
      :<|> listTags
      :<|> getEvent
      :<|> getParticipants
      :<|> getPictures
      :<|> mkEvent
      :<|> rmEvent
      :<|> joinEvent
      :<|> postComment
      :<|> getcomments
      :<|> searchEventsWithTag
      :<|> getEventTags


listEvents      = query $ select events
listTags        = query $ select tags
getEvent        = getByIDM events        #eid
getParticipants = getByID  participants  #eid
getPictures     = getByID  pictures      #eid
mkEvent MkEventReq {..} = do
  ownerID <- gets AppM.uid

  cid <- insertWithPK chats
    [Chat def True]

  evid <- insertWithPK events
    [Event def ownerID req_title req_desc req_place req_date cid]

  insert_ participants
    [Participants evid ownerID Going]

  ts <- fmap catMaybes . forM (S.toList req_tags) $ get1 tags #tid
  insert_ eventtags $ EventTags evid <$> ts

  pure evid

rmEvent :: EventID -> AppM Private Bool
rmEvent eid = do
  myId <- getID
  es <- query . from #eid $ suchThat (select events)
          \e -> e ! #eid   .== literal eid
            .&& e ! #owner .== literal myId

  forM_ es $ \e -> do
    -- TODO: Delete chat
    deleteFrom_ participants \p  -> p  ! #eid .== literal e
    deleteFrom_ events       \e' -> e' ! #eid .== literal e

  pure . not $ null es


joinEvent (evid, lop) = do
  uid <- gets AppM.uid

  -- Does the event exist?
  Just _ <- get1 events #eid evid

  upsert participants
    (\p -> p ! #eid .== literal evid .&& p ! #uid .== literal uid)
    (flip with [ #lop := literal lop ])
    [Participants evid uid lop]

  pure NoContent

postComment (eid, txt) = do
  ownerID <- gets AppM.uid
  cid <- insertWithPK comments
    [ Comments def eid ownerID txt ]
  pure cid

getcomments (eid) = query do
  e <- select comments
  restrict $ e ! #eid .== literal eid
  pure e

getEventTags eid = query do
  t <- select eventtags
  restrict $ t ! #eid .== literal eid
  g <- select tags
  restrict $ g ! #tid .== t ! #tid
  pure g


--------------



data EventQuery
  = ByUser UserID
  | ByText Text
  deriving Show

parseQuery :: Text -> EventQuery
parseQuery str = fromMaybe (ByText $ "%" <> str <> "%") byUser where
  byUser = do
    guard $ T.isPrefixOf "user:" str
    let (d, _) = T.span isDigit . T.stripStart $ T.drop 5 str
    guard . not $ T.null d
    pure . ByUser . toId . read $ T.unpack d


searchEventsWithTag :: (Text, TagSearch) -> AppM Private [Event]
searchEventsWithTag (q, Nothing) = searchevents q $ select events
searchEventsWithTag (q, Just ts) = searchevents q $ do
  t <- select eventtags
  restrict $ t ! #tid .== literal ts
  e <- select events
  restrict $ e ! #eid .== t ! #eid
  pure e


searchevents (parseQuery->q) events = case q of
  ByUser uid -> eventsByOwner events uid
  ByText txt -> query do
    e <- events
    restrict $ e ! #desc  `like` literal txt
           .|| e ! #desc  `like` literal txt
           .|| e ! #place `like` literal txt
    pure e

eventsByOwner events uid = query do
  e <- events
  restrict $ e ! #owner .== literal uid
  pure e

