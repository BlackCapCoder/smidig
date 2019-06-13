module Tags where

import Utils

#define KitchenSink Eq, Show, Generic, ToJSON, FromJSON
#define Row (KitchenSink, SqlRow)


type TagID = ID Tags
data Tags  = Tags
  { tid    :: TagID
  , name   :: Text
  } deriving Row

tags :: Table Tags
tags = table "tags" [#tid :- autoPrimary]


