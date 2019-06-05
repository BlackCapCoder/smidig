module TagsBE where

import Tags
import Utils
import AppM


instance Backend Tags where
  type Acc Tags = Private
  type API Tags
     = "tags" :> Get '[JSON] [Tags]

  server = getTags


getTags = query $ select tags

