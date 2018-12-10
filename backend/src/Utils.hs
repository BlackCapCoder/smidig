module Utils
  ( module Utils
  , module Servant
  , module Database.Selda
  , module Database.Selda.SQLite
  , module Data.Aeson
  , module Control.Monad
  , module Data.Maybe
  , Text
  , Generic
  ) where

import Servant
import Database.Selda
import Database.Selda.SQLite

import Data.Aeson hiding (Result)
import Data.Text    (Text)
import GHC.Generics (Generic)

import Control.Monad
import Data.Maybe


instance ToJSON (ID a) where
  toJSON = toJSON . fromId

instance FromJSON (ID a) where
  parseJSON = fmap toId . parseJSON

instance FromHttpApiData (ID a) where
  parseQueryParam = fmap toId . parseQueryParam


getByID db t sel (Just id) = db . query $ do
  x <- select t
  restrict (x ! sel .== literal id)
  return x

getByIDM db t sel = fmap listToMaybe . getByID db t sel

