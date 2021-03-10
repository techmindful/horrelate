{-# language OverloadedStrings #-}

module Utils.Derives where

import           DearImGui ( ImVec2(..) )

import           Data.Aeson


instance FromJSON ImVec2 where
  parseJSON (Object v) = ImVec2 <$> v .: "x" <*> v .: "y"
instance Show ImVec2 where
  show ( ImVec2 { x = x', y = y' } ) = "ImVec2 { x = " ++ show x' ++ ", y = " ++ show y' ++ " }"

