{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module Types where

import           DearImGui ( ImVec2(..) )

import           Data.Aeson

import           Data.Generics.Labels
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           GHC.Generics


-- ImVec2 additional classes
instance FromJSON ImVec2 where
  parseJSON (Object v) = ImVec2 <$> v .: "x" <*> v .: "y"
instance Show ImVec2 where
  show ( ImVec2 { x = x', y = y' } ) = "ImVec2 { x = " ++ show x' ++ ", y = " ++ show y' ++ " }"

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
} deriving ( Generic, Show )
instance FromJSON Node


data Save = Save {
  allActivityNames :: [ String ]
, nodes            :: [ Node ]
} deriving ( Generic )
instance FromJSON Save


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2
