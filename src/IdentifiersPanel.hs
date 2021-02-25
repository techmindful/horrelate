{-# language OverloadedLabels #-}

module IdentifiersPanel ( drawIdentifiersPanel ) where

import           Types
import qualified Utils

import qualified DearImGui
import           DearImGui ( ImVec2(..) )

import           Control.Exception ( bracket )
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad.State
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Function ( (&) )
import           Data.Map.Strict as Map

drawIdentifiersPanel :: StateT AppState IO ()
drawIdentifiersPanel = do

  appState <- get

  liftIO $ Utils.setCursorPos' ( appState & cursorPosRef ) ( ImVec2 720 300 )

  liftIO $ newIORef ( ImVec2 560 300 ) >>= DearImGui.beginChildOfSize "All Identifiers"

  isComboOpen <- liftIO $ DearImGui.beginCombo "Identifier Type" ( appState ^. #identifierTypeSel )
  case isComboOpen of
    False -> return ()
    True  -> do
      let allIdentifierTypes = ( appState ^. #appData . #allIdentifiers ) & Map.keys
      appState' <- liftIO $ execStateT ( mapM_ drawIdentifierType allIdentifierTypes ) appState
      put appState'
      liftIO $ DearImGui.endCombo
          
  liftIO $ DearImGui.endChild

drawIdentifierType :: String -> StateT AppState IO ()
drawIdentifierType typeStr = do

  appState <- get

  isSelected <- DearImGui.selectable typeStr
  case isSelected of
    False -> return ()
    True  -> do
      put $ ( appState & #identifierTypeSel .~ typeStr )

