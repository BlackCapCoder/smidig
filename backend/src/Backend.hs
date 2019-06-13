module Backend
  ( app
  )
  where

import Utils
import AppM hiding (app)

import User
import File
import JsApi
import Login
import Event as E
import Chat  as C
import Notifications
import Tags
import TagsBE
import Group as G


-- Higher kinded associated type families works in
-- the repl, but not when compiled..
type family MapAPI xs where
  MapAPI '[] = '[]
  MapAPI (x ': xs) = API x : MapAPI xs

-- Dereference generic variable
data SomeTable where SomeTable :: Table a -> SomeTable

-- All tables in the database
tables :: [SomeTable]
tables =
  [ SomeTable users
  , SomeTable events
  , SomeTable E.participants
  , SomeTable pictures
  , SomeTable chats
  , SomeTable C.participants
  , SomeTable chatMessages
  , SomeTable E.comments
  , SomeTable favorites
  , SomeTable friends
  , SomeTable notifications
  , SomeTable tags
  , SomeTable E.eventtags
  , SomeTable G.groups
  , SomeTable G.grouptags
  , SomeTable G.groupMembers
  , SomeTable G.comments
  ]

type Modules =
  [ User
  , LoggedUser
  , Event
  , Group
  , Chat
  , Notification
  , Tags
  , WhoAmI
  ]

type REST = Concat
  [ Concat Modules
  , JsApi "api.js" Public ( Concat
    [ Concat (MapAPI Modules)
    , Login
    ])
  ]

-- Things that should be private
type Priv = Concat
  [ File Private "../frontend/desktop.html"  "desktop"
  , File Private "../frontend/event.html"    "event"
  , File Private "../frontend/events.html"   "events"
  , File Private "../frontend/group.html"    "groupPage"
  , File Private "../frontend/groups.html"   "groups"
  , File Private "../frontend/friends.html"  "friends"
  , File Private "../frontend/messages.html" "messages"
  , File Private "../frontend/mkevent.html"  "mkevent"
  , File Private "../frontend/mkgroup.html"  "mkgroup"
  , File Private "../frontend/profile.html"  "profile"
  , File Private "../frontend/settings.html" "settings"
  , File Private "../frontend/changes.html"  "changes"
  ]

-- Things that should be public
type Pub = File' "../frontend/login.html"    Public
      -- :<|> File' "../frontend/register.html" Public

-- The entire backend
type Full = REST :<|> Priv :<|> Pub


-- Entry point
app :: IO Application
app = do

  -- Create tables that doesn't exist
  database $
    forM_ tables \(SomeTable t) -> tryCreateTable t

  toApp @Full

