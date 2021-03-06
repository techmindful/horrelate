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
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map(..) )
import           GHC.Generics


-- ImVec2 additional classes
instance FromJSON ImVec2 where
  parseJSON (Object v) = ImVec2 <$> v .: "x" <*> v .: "y"
instance Show ImVec2 where
  show ( ImVec2 { x = x', y = y' } ) = "ImVec2 { x = " ++ show x' ++ ", y = " ++ show y' ++ " }"

data AppState = AppState {

  appData :: AppData

, editingService :: Maybe String
, serviceNameEditRef :: IORef String

, identifierTypeSel :: Maybe String
, editingIdentifierValue :: Maybe String
, identifierValueEditRef :: IORef String

, nodeEdit :: Maybe NodeEdit

, nodeActNameEditRef :: IORef String

, cursorPosRef :: IORef ImVec2
} deriving ( Generic )


data AppData = AppData {
  allServiceNames :: [ String ]
, allIdentifiers  :: Map String [ String ]
, nodes           :: [ Node ]
} deriving ( Generic, Show )


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
  | IdentField
  deriving ( Eq, Generic, Ord )


instance FromJSON AppData
data Command
  = Add
  | Quit
  deriving ( Eq, Show )


data Activity = Activity {
  name :: String
, service :: String
, reg :: Registration
} deriving ( Eq, Show, Generic )
instance FromJSON Activity


data Registration = Registration {
    email    :: String
  , phoneNum :: String
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


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2
