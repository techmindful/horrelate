module Types where


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

