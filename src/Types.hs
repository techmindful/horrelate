{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLabels #-}

module Types where

import           DearImGui ( ImVec2(..) )

import           Data.Aeson

import           Data.Generics.Labels
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           GHC.Generics


data AppState = AppState {
  -- Data
  allActivityNames :: [ String ]
  -- View
, cursorPosRef :: IORef ImVec2
, editingActivity :: Maybe Int
, activityNameEditRef :: IORef String
} deriving ( Generic )


data Command
  = Add
  | Quit
  deriving ( Eq, Show )


data Activity = Activity {
  reg :: Registration
} deriving ( Eq, Show, Generic )
instance FromJSON Activity

data Registration = Registration {
    email    :: String
  , phoneNum :: Int
  --, name     :: Name
  --, address  :: Address
} deriving ( Eq, Show, Generic )
instance FromJSON Registration

newtype Email = Email String
  deriving ( Eq, Show, Generic )
instance FromJSON Email

newtype PhoneNum  = PhoneNum Int
  deriving ( Eq, Show, Generic )
instance FromJSON PhoneNum

data Name         = Name {
    firstName :: String
  , midName   :: String
  , lastName  :: String
} deriving ( Eq, Show, Generic )
instance FromJSON Name

data Address      = Address {
    street  :: String
  , apt     :: String
  , city    :: String
  , country :: String 
} deriving ( Eq, Show, Generic )
instance FromJSON Address


data Node = Node {
  activity :: Activity
, drawPos  :: ImVec2
}


data Save = Save {
  allActivityNames :: [ String ]
, nodes            :: [ Node ]
} deriving ( Generic )


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2
