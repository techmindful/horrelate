{-# language DeriveAnyClass #-}
{-# language DeriveGeneric #-}
{-# language DuplicateRecordFields #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}

module Types where

import           Activity
import           Node

import           DearImGui ( ImVec2(..) )

import           Data.Aeson

import           Data.Generics.Labels
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map.Strict as Map
import           Data.Map.Strict ( Map(..) )
import           GHC.Generics


data AppState = AppState {

  appData :: AppData

, editingService :: Maybe String
, serviceNameEditRef :: IORef String

, identifierTypeSel :: Maybe String
, editingIdentifierValue :: Maybe String
, identifierValueEditRef :: IORef String

, nodeEdit :: Maybe NodeEdit

, nodeActNameEditRef :: IORef String
, nodeServEdit :: String
, nodeIdentTypeEdit :: Maybe String
, nodeIdentValEdit  :: Maybe String
, nodeIdentEdit :: IdentEdit

, cursorPosRef :: IORef ImVec2
} deriving ( Generic )


data AppData = AppData {
  allServiceNames :: [ String ]
, allIdentifiers  :: Map String [ String ]
, nodes           :: [ Node ]
} deriving ( Generic, Show )
instance FromJSON AppData


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2
