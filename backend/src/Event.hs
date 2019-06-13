module Event where

import Utils
import AppM
import Tags
import qualified Data.Text as T
import Data.Char
import qualified Data.Set as S
import Data.Maybe
import Chat (ChatID, chats, Chat (..))

#define KitchenSink Eq, Show, Generic, ToJSON, FromJSON
#define Row (KitchenSink, SqlRow)


type EventID = ID Event
data Event   = Event
  { eid      :: EventID
  , owner    :: UserID
  , title    :: Text
  , subtitle :: Text
  , desc     :: Text
  , place    :: Text
  , date     :: UTCTime
  , cid      :: ChatID
  } deriving Row

events :: Table Event
events = table "events" [#eid :- autoPrimary]


data Participants = Participants
  { eid :: EventID
  , uid :: UserID
  , lop :: LevelOfParticipation
  } deriving Row

data LevelOfParticipation
  = Interested | Going
  deriving (KitchenSink, SqlType, Bounded, Enum, Read)

participants :: Table Participants
participants = table "participants" []


data Pictures = Pictures
  { eid  :: EventID
  , pth  :: Text
  } deriving Row

pictures :: Table Pictures
pictures = table "pictures" []


type CommentID = ID Comments
data Comments  = Comments
  { cid     :: CommentID
  , eid     :: EventID
  , owner   :: UserID
  , content :: Text
  } deriving Row

comments :: Table Comments
comments = table "comments" [#cid :- autoPrimary]


data EventTags = EventTags
  { eid :: EventID
  , tid :: TagID
  } deriving Row


eventtags :: Table EventTags
eventtags = table "eventtags" []


-------------------------


data MkEventReq = MkEventReq
  { req_title :: Text
  , req_sub   :: Text
  , req_desc  :: Text
  , req_place :: Text
  , req_date  :: UTCTime
  , req_tags  :: S.Set TagID
  } deriving (KitchenSink)

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
    :<|> "myEvents"      :> Get '[JSON] [Event]

  server = listEvents
      :<|> listTags
      :<|> getEvent
      :<|> getParticipants
      :<|> getPictures
      :<|> mkEvent
      :<|> rmEvent
      :<|> joinEvent
      :<|> postComment
      :<|> getComments
      :<|> searchEventsWithTag
      :<|> getEventTags
      :<|> myEvents


listEvents      = query $ select events
listTags        = query $ select tags
getEvent        = getByIDM events        #eid
getParticipants = getByID  participants  #eid
getPictures     = getByID  pictures      #eid
getComments     = getByID' comments      #eid

mkEvent MkEventReq {..} = do
  ownerID <- getID

  cid <- insertWithPK chats
    [Chat def True]

  evid <- insertWithPK events
    [Event def ownerID req_title req_sub req_desc req_place req_date cid]

  insert_ participants
    [Participants evid ownerID Going]

  ts <- fmap catMaybes . forM (S.toList req_tags) $ get1 tags #tid
  insert_ eventtags $ EventTags evid <$> ts

  pure evid

rmEvent :: EventID -> AppM Private Bool
rmEvent eid = do
  myId <- getID
  es <- query . from #eid $ events `having`
          and' [#eid ?= eid, #owner ?= myId]

  forM_ es \e -> do
    -- TODO: Delete chat
    deleteFrom_ participants $ #eid ?= e
    deleteFrom_ events       $ #eid ?= e

  pure . not $ null es


joinEvent (evid, lop) = do

  -- Does the event exist?
  Just _ <- get1 events #eid evid

  uid <- getID
  upsert participants
    (and' [#eid ?= evid, #uid ?= uid])
    (flip with [ #lop := literal lop ])
    [Participants evid uid lop]

  pure NoContent

postComment (eid, txt) = do
  ownerID <- getID
  insertWithPK comments
    [ Comments def eid ownerID txt ]



getEventTags eid = query do
  t <- eventtags `having` #eid ?= eid
  g <- select tags
  restrict $ g ! #tid .== t ! #tid
  pure g

myEvents = do
  myId <- getID
  query do
    p <- participants `having` #uid ?= myId
    e <- select events
    restrict $ e ! #eid .== p ! #eid
    pure e



--------------



data EventQuery
  = ByUser UserID
  | ByText Text

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
  t <- eventtags `having` #tid ?= ts
  e <- select events
  restrict $ e ! #eid .== t ! #eid
  pure e


searchevents (parseQuery->q) events = case q of
  ByUser uid -> eventsByOwner events uid
  ByText txt -> query $
    events `suchThat`
      \e -> e ! #desc  `like` literal txt
        .|| e ! #desc  `like` literal txt
        .|| e ! #place `like` literal txt

eventsByOwner events uid
  = query $ events `suchThat` #owner ?= uid

