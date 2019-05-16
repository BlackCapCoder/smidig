module Backend
  ( app
  , Full
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


-- Things that we should generate a javascript API for
type REST = Flatten (:<|>)
  [ UserB, WhoAmI, ChatB
  , JsApi "api.js" Public
      ( API UserB
   :<|> API EventB
   :<|> API WhoAmI
   :<|> API ChatB
   :<|> Login
      )
  ]

-- Things that should be private
type Priv = Flatten (:<|>)
  [ File "../frontend/desktop.html"  "desktop.html"  Private
  , File "../frontend/event.html"    "event.html"    Private
  , File "../frontend/events.html"   "events.html"   Private
  , File "../frontend/friends.html"  "friends.html"  Private
  , File "../frontend/messages.html" "messages.html" Private
  , File "../frontend/mkevent.html"  "mkevent.html"  Private
  , File "../frontend/profile.html"  "profile.html"  Private
  , File "../frontend/settings.html" "settings.html" Private
  ]

-- Things that should be public
type Pub = Flatten (:<|>)
  [ EventB
  , File' "../frontend/login.html" Public
  ]


-- The entire backend
type Full = REST :<|> Priv :<|> Pub


initDb :: IO ()
initDb =
  database do tryCreateTable users
              tryCreateTable events
              tryCreateTable E.participants
              tryCreateTable pictures
              tryCreateTable chats
              tryCreateTable E.participants
              tryCreateTable chatMessages

app = initDb >> toApp @Full
