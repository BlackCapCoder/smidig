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

