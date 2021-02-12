module Types where

import           DearImGui ( ImVec2(..) )

import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )


data Activity = Activity {
  reg :: Registration
}

data Registration = Registration {
    email    :: Email
  , phoneNum :: PhoneNum
  , name     :: Name
  , address  :: Address
}

newtype Email     = Email String
newtype PhoneNum  = PhoneNum Int
data Name         = Name {
    firstName :: String
  , midName   :: String
  , lastName  :: String
}
data Address      = Address {
    street  :: String
  , apt     :: String
  , city    :: String
  , country :: String 
}

data Command
  = Add String
  | Quit


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2
