module Backend
  ( app
  )
  where

import Utils

import User
import File
import JsApi
import Login
import Event as E
import Chat  as C

import AppM hiding (app)


-- Dereference generic variable
data SomeTable where SomeTable :: Table a -> SomeTable

-- All tables in the databases
tables :: [SomeTable]
tables =
  [ SomeTable users
  , SomeTable events
  , SomeTable E.participants
  , SomeTable pictures
  , SomeTable chats
  , SomeTable C.participants
  , SomeTable chatMessages
  , SomeTable comments
  ]


-- Things that we should generate a javascript API for
type REST = Flatten (:<|>)
  [ User
  , Event
  , Chat
  , WhoAmI
  , JsApi "api.js" Public
      ( API User
   :<|> API Event
   :<|> API WhoAmI
   :<|> API Chat
   :<|> Login
      )
  ]

-- Things that should be private
type Priv = Flatten (:<|>)
  [ File "../frontend/desktop.html"  "desktop"  Private
  , File "../frontend/event.html"    "event"    Private
  , File "../frontend/events.html"   "events"   Private
  , File "../frontend/friends.html"  "friends"  Private
  , File "../frontend/messages.html" "messages" Private
  , File "../frontend/mkevent.html"  "mkevent"  Private
  , File "../frontend/profile.html"  "profile"  Private
  , File "../frontend/settings.html" "settings" Private
  ]

-- Things that should be public
type Pub = File' "../frontend/login.html" Public

-- The entire backend
type Full = REST :<|> Priv :<|> Pub


-- Entry point
app :: IO Application
app = do

  -- Initialize database
  database $
    forM_ tables \(SomeTable t) -> tryCreateTable t

  toApp @Full
