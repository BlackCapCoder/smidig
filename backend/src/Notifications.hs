module Notifications where

import Utils
import AppM

#define KitchenSink Eq, Show, Generic, ToJSON, FromJSON
#define Row (KitchenSink, SqlRow)


type NotificationID = ID Notification

data Notification = Notification
  { nid  :: NotificationID
  , uid  :: UserID
  , seen :: Bool
  , kind :: Kind
  , key  :: Int -- Some ID to a table determined by kind
  }
  deriving Row

data Kind
  = FriendReq
  | UnknownNotification
  | GroupInvitation
  deriving (KitchenSink, Bounded, Enum, Read, SqlType)

notifications :: Table Notification
notifications = table "notifications" [#nid :- autoPrimary]

rmNotification :: NotificationID -> AppM Private ()
rmNotification nid = do
  myid <- getID
  deleteFrom_ notifications $ and'
    [ #nid ?= nid
    , #uid ?= myid
    ]


notify :: UserID -> Kind -> ID a -> AppM Private ()
notify to t k =
  insert_ notifications
    [ Notification def to False t $ fromId k ]



instance Backend Notification where
  type Acc Notification = Private
  type API Notification
    = "notifications" :> Get '[JSON] [Notification]

  server = getNotifications

getNotifications :: AppM Private [Notification]
  = query . having notifications . is #uid =<< getID

