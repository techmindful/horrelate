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
import           Data.Maybe ( fromMaybe )


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
  let comboActiveStr = fromMaybe "<No identifier type selected>" $ appState ^. #identifierTypeSel
  isComboOpen <- liftIO $ DearImGui.beginCombo "##Identifier Type" comboActiveStr
  case isComboOpen of
    False -> return ()
    True  -> do
      let allIdentifierTypes = ( appState ^. #appData . #allIdentifiers ) & Map.keys
      ( liftIO $ execStateT ( mapM_ drawType allIdentifierTypes ) appState ) >>= put
      liftIO $ DearImGui.endCombo

  appState' <- get

  -- Draw values.
  let maybeValues = ( appState' ^. #identifierTypeSel ) >>= ( \sel -> Map.lookup sel ( appState' ^. #appData . #allIdentifiers ) )
  case maybeValues of
    Nothing -> return ()
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
      put ( appState & #identifierTypeSel .~ Just typeStr )
      -- Also cancel any editing of identifier value.
    >> get
    >>= ( \state -> put ( state & #editingIdentifierValue .~ Nothing ) )


drawValue :: ( Int, String ) -> StateT AppState IO ()
drawValue ( i, value ) = do

  appState <- get

  let pos_X = x valuesStartPos
      pos_Y = y valuesStartPos + ( fromIntegral i ) * valuesGap_Y
  Utils.setCursorPos' ( appState & cursorPosRef ) $ ImVec2 pos_X pos_Y
  -- TODO: Using same ref now causes editing all input boxes.
  DearImGui.inputText ( "##Identifier Value Edit " ++ show i ) ( appState & identifierValueEditRef ) 128

  return ()

