{-# language DeriveGeneric #-}

module Node where

import           Activity
import           Utils.Derives

import           DearImGui ( ImVec2(..) )

import           Data.Aeson

import           GHC.Generics


data Node = Node {
  activity :: Activity
, drawPos  :: ImVec2
} deriving ( Generic, Show )
instance FromJSON Node


data NodeEdit = NodeEdit {
  actName :: String
, field   :: NodeField
} deriving ( Generic )


data NodeField
  = ActField
  | ServField
  | IdentField String
  deriving ( Eq, Generic, Ord )


data IdentEdit
  = NoEdit
  | IdentEdit String ( Maybe String )

