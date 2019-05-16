module Backend
  ( app
  , Full
  )
  where

import Utils

import Event
import User
import File
import JsApi
import Login

import AppM hiding (app)


-- Things that we should generate a javascript API for
type REST = Flatten (:<|>)
  [ UserB
  , JsApi "api.js" Public (API UserB :<|> API EventB :<|> Login)
  ]

-- Things that should be public
type Pub = Flatten (:<|>)
  [ Folder "../frontend/css/"       "css"        Public
  , Folder "../frontend/js/"        "js"         Public
  , Folder "../frontend/imgs/"      "imgs"       Public
  , File'  "../frontend/login.html" Public
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
  , File "../frontend/register.html" "register.html" Private
  , File "../frontend/settings.html" "settings.html" Private
  ]

-- The entire backend
type Full = REST :<|> Pub :<|> Priv :<|> EventB


initDb :: IO ()
initDb =
  database do tryCreateTable users
              tryCreateTable events
              tryCreateTable participants
              tryCreateTable pictures

app = initDb >> toApp @Full
