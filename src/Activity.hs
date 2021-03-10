{-# language DeriveGeneric #-}

module Activity where

import           Data.Aeson

import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map(..) )
import           GHC.Generics


data Activity = Activity {
  name :: String
, service :: String
, identifiers :: Map String String
} deriving ( Eq, Show, Generic )
instance FromJSON Activity

