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



data ChatB

instance Backend ChatB where
  type Acc ChatB = Private
  type API ChatB = "myChats"  :> Get '[JSON] [Chat]
              :<|> "mkChat"   :> ReqBody '[JSON] (Bool, Set UserID)
                              :> Post '[JSON] ChatID
              :<|> "putChat"  :> ReqBody '[JSON] (ChatID, Text)
                              :> Post '[JSON] ChatMessageID
              :<|> "readChat" :> ReqBody '[JSON] ChatID
                              :> Post '[JSON] [ChatMessage]

  server = myChats
      :<|> mkChat
      :<|> putChat
      :<|> readChat


chid = cid :: Chat -> ChatID

readChat :: ChatID -> AppM Private [ChatMessage]
readChat chat = do
  [c] <- filter ((chat ==) . chid) <$> myChats
  getByID' chatMessages #cid $ chid c

putChat :: (ChatID, Text) -> AppM Private ChatMessageID
putChat (chat, msg) = do
  myID <- asks AppM.uid
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
  myID <- asks AppM.uid
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
  myID <- asks AppM.uid
  query do
    p <- select participants
    restrict $ p ! #uid .== literal myID
    c <- select chats
    restrict $ c ! #cid .== p ! #cid
    pure c
