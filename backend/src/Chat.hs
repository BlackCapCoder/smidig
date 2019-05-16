module Chat where

import Utils
import AppM


type ChatID = ID Chat

data Chat = Chat
  { id           :: ChatID
  , owner        :: UserID
  , participants :: [UserID] -- Set?
  }

data ChatMessage = ChatMessage
  { chat    :: ChatID
  , sender  :: UserID
  , content :: Text
  }


data ChatB

-- instance Backend ChatB where
--   type API ChatB = EmptyAPI
--   server = undefined
