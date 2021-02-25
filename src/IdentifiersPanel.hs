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


panelPos  = ImVec2 720 300
panelSize = ImVec2 560 300

-- Relative to child window.
valuesStartPos = ImVec2 0 40
valuesGap_Y    = 20.0

drawIdentifiersPanel :: StateT AppState IO ()
drawIdentifiersPanel = do

  appState <- get

  liftIO $ Utils.setCursorPos' ( appState & cursorPosRef ) panelPos
  liftIO $ newIORef panelSize >>= DearImGui.beginChildOfSize "All Identifiers"

  -- Draw combo.
  isComboOpen <- liftIO $ DearImGui.beginCombo "Identifier Type" ( appState ^. #identifierTypeSel )
  case isComboOpen of
    False -> return ()
    True  -> do
      let allIdentifierTypes = ( appState ^. #appData . #allIdentifiers ) & Map.keys
      ( liftIO $ execStateT ( mapM_ drawType allIdentifierTypes ) appState ) >>= put
      liftIO $ DearImGui.endCombo

  appState' <- get

  -- Draw values.
  let maybeValues = Map.lookup ( appState' ^. #identifierTypeSel ) ( appState' ^. #appData . #allIdentifiers )
  case maybeValues of
    Nothing -> do
      liftIO $ putStrLn "Error: No identifier values found given the selected type in the combo."
    Just values -> do
      let indexedValues = zip [ 0.. ] values
      ( liftIO $ execStateT ( mapM_ drawValue indexedValues ) appState' ) >>= put

  liftIO $ DearImGui.endChild

drawType :: String -> StateT AppState IO ()
drawType typeStr = do

  appState <- get

  isSelected <- DearImGui.selectable typeStr
  case isSelected of
    False -> return ()
    True  -> do
      put $ ( appState & #identifierTypeSel .~ typeStr )


drawValue :: ( Int, String ) -> StateT AppState IO ()
drawValue ( i, value ) = do

  appState <- get

  let pos_X = x valuesStartPos
      pos_Y = y valuesStartPos + ( fromIntegral i ) * valuesGap_Y
  Utils.setCursorPos' ( appState & cursorPosRef ) $ ImVec2 pos_X pos_Y
  DearImGui.text value

