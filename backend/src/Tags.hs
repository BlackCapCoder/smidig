module Tags where

import Utils


type TagID = ID Tags
data Tags  = Tags
  { tid    :: TagID
  , name   :: Text
  , icon   :: Maybe Text
  , banner :: Maybe Text
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, SqlRow)

tags :: Table Tags
tags = table "tags" [#tid :- autoPrimary]


