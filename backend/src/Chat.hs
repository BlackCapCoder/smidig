module Chat where

import Utils hiding (Set)
import AppM hiding (Set)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Foldable


type ChatID = ID Chat

data Chat = Chat
  { cid      :: ChatID
  , isPublic :: Bool
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

chats :: Table Chat
chats = table "chats" [#cid :- autoPrimary]


data Participants = Participants
  { cid :: ChatID
  , uid :: UserID
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

participants :: Table Participants
participants = table "chatparticipants" []


type ChatMessageID = ID ChatMessage

data ChatMessage = ChatMessage
  { mid     :: ChatMessageID
  , cid     :: ChatID
  , sender  :: UserID
  , content :: Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

chatMessages :: Table ChatMessage
chatMessages = table "chatmessages" [#mid :- autoPrimary]


instance Backend Chat where
  type Acc Chat = Private
  type API Chat
       = "myChats"  :> Get '[JSON] [Chat]
    :<|> "mkChat"   :> (Bool, Set UserID) ~> ChatID
    :<|> "putChat"  :> (ChatID, Text)     ~> ChatMessageID
    :<|> "readChat" :> ChatID             ~> [ChatMessage]
    :<|> "readChatSince" :> (ChatID, ChatMessageID) ~> [ChatMessage]
    :<|> "chatWithUser"  :> (UserID) ~> ChatID

  server = myChats
      :<|> mkChat
      :<|> putChat
      :<|> readChat
      :<|> readChatSince
      :<|> chatWithUser


chatWithUser :: UserID -> AppM Private ChatID
chatWithUser uid = do

  -- TODO: Use aggregate
  cs <- myChats >>= filterM \c -> do
    (==1) . length <$> query do
      p <- select participants
      restrict $ p ! #cid .== literal (chid c)
             .&& p ! #uid .== literal uid
      pure p

  if | null cs   -> mkChat (False, S.fromList [uid])
     | [c] <- cs -> pure $ chid c


chid = cid :: Chat -> ChatID

readChatSince :: (ChatID, ChatMessageID) -> AppM Private [ChatMessage]
readChatSince (chat, since) = filter f <$> readChat chat
  where f msg = mid msg > since

readChat :: ChatID -> AppM Private [ChatMessage]
readChat chat = do
  [c] <- filter ((chat ==) . chid) <$> myChats
  getByID' chatMessages #cid $ chid c

putChat :: (ChatID, Text) -> AppM Private ChatMessageID
putChat (chat, msg) = do
  myID <- gets AppM.uid
  _:_  <- query do
    c <- select chats
    restrict $ c ! #cid .== literal chat
    p <- select participants
    restrict $ p ! #cid .== c ! #cid .&& p ! #uid .== literal myID
    pure p

  mid <- insertWithPK chatMessages
    [ ChatMessage def chat myID msg ]

  pure mid

mkChat :: (Bool, Set UserID) -> AppM Private ChatID
mkChat (isPub, uids') = do
  myID <- gets AppM.uid
  let uids = S.delete myID uids'
  False <- pure $ null uids

  us <- query $ distinct do
    u <- select users
    restrict $ (u ! #uid) `isIn` (literal <$> S.toList uids)
    pure $ u ! #uid

  False <- pure $ null us

  chat <- insertWithPK chats
    [Chat def isPub]

  insert_ participants . flip fmap (myID:us) $
    Participants chat

  pure chat


myChats :: AppM Private [Chat]
myChats = do
  myID <- gets AppM.uid
  query do
    p <- select participants
    restrict $ p ! #uid .== literal myID
    c <- select chats
    restrict $ c ! #cid .== p ! #cid
    pure c
