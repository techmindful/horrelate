{-# LANGUAGE DeriveGeneric #-}

module Types where

import           DearImGui ( ImVec2(..) )

import           Data.Aeson

import           GHC.Generics
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )


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

data Command
  = Add
  | Quit
  deriving ( Eq, Show )


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2
