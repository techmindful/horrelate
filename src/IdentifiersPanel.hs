{-# language OverloadedLabels #-}

module IdentifiersPanel ( drawIdentifiersPanel ) where

import           Types
import qualified Utils

import qualified DearImGui
import           DearImGui ( ImVec2(..) )

import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad.State
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Function ( (&) )
import           Data.Map.Strict as Map

drawIdentifiersPanel :: StateT AppState IO ()
drawIdentifiersPanel = do

  appState <- get

  liftIO $ do

    Utils.setCursorPos' ( appState & cursorPosRef ) ( ImVec2 720 300 )
    newIORef ( ImVec2 560 300 ) >>= DearImGui.beginChildOfSize "All Identifiers"
    isComboOpen <- DearImGui.beginCombo "Identifier Type" "Test"
    case isComboOpen of
      False -> return ()
      True  -> do
        mapM_ DearImGui.selectable $ ( appState ^. #appData . #allIdentifiers ) & Map.keys
        DearImGui.endCombo
    DearImGui.endChild

