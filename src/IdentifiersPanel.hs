{-# language LambdaCase #-}
{-# language OverloadedLabels #-}

module IdentifiersPanel ( drawIdentifiersPanel ) where

import           Types
import qualified Utils

import qualified DearImGui
import           DearImGui ( ImVec2(..) )

import           Control.Error.Util
import           Control.Exception ( bracket )
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad.Except
import           Control.Monad.State
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Function ( (&) )
import qualified Data.Map.Strict as Map
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

  -- Draw values.
  let
    drawValues :: ExceptT ( IO () ) ( StateT AppState IO ) ()
    drawValues = do
      
      appState <- lift get

      typeSel <- failWith ( return () ) ( appState ^. #identifierTypeSel )

      values <-
        failWith
          ( putStrLn "Error: Identifier values lookup fails with selected type." )
          ( Map.lookup typeSel $ appState ^. #appData . #allIdentifiers )

      let indexedValues = zip [ 0.. ] values
          toRun = sequence_ $ map ( $ typeSel ) $ map drawValue indexedValues

      lift $ put =<< ( liftIO $ execStateT toRun appState )

  runExceptT drawValues >>= \case
    Right _ -> return ()
    Left m  -> liftIO m

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


drawValue :: ( Int, String ) -> String -> StateT AppState IO ()
drawValue ( i, value ) typeSel = do

  appState <- get

  let textPos_X = x valuesStartPos
      textPos_Y = y valuesStartPos + ( fromIntegral i ) * valuesGap_Y
      textPos   = ImVec2 textPos_X textPos_Y

      delButtonPos_X = x panelSize - 40
      delButtonPos_Y = textPos_Y
      delButtonPos   = ImVec2 delButtonPos_X delButtonPos_Y

      editButtonPos_X = delButtonPos_X - 40
      editButtonPos_Y = textPos_Y
      editButtonPos   = ImVec2 editButtonPos_X editButtonPos_Y

      cancelButtonPos  = delButtonPos
      confirmButtonPos = editButtonPos
      inputPos         = textPos

  let cursorPosRef' = appState & cursorPosRef

  case fmap ( == value ) ( appState ^. #editingIdentifierValue ) of
    Just True -> do
      Utils.setCursorPos' cursorPosRef' inputPos
      DearImGui.inputText ( "##Identifier Value Edit " ++ show i ) ( appState & identifierValueEditRef ) 128
      return ()

    _ -> do
      Utils.setCursorPos' cursorPosRef' textPos
      DearImGui.text value

      Utils.setCursorPos' cursorPosRef' editButtonPos
      DearImGui.button ( "Edit##" ++ show i ) >>= \case
        True  -> put $ appState & #editingIdentifierValue .~ Just value
        False -> return ()

      Utils.setCursorPos' cursorPosRef' delButtonPos
      DearImGui.button ( "Del##" ++ show i ) >>= \case
        True  -> put $ appState & #appData . #allIdentifiers %~ Map.adjust ( filter ( /= value ) ) typeSel
        False -> return ()

  return ()

