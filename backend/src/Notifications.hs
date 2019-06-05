module Notifications where

import Utils
import AppM


type NotificationID = ID Notification

data Notification = Notification
  { nid  :: NotificationID
  , uid  :: UserID
  , seen :: Bool
  , kind :: Kind
  , key  :: Int -- Some ID to a table determined by kind
  }
  deriving (Eq, Generic, ToJSON, FromJSON, SqlRow)

notifications :: Table Notification
notifications = table "notifications" [#nid :- autoPrimary]

data Kind
  = FriendReq
  | UnknownNotification
  | GroupInvitation
  deriving ( Eq, Bounded, Enum, Show, Read
           , Generic, ToJSON, FromJSON, SqlType
           )


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
getNotifications = do
  myid <- gets AppM.uid

  query do
    n <- select notifications
    restrict $ n ! #uid .== literal myid
    pure n

