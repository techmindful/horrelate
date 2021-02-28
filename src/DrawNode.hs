{-# language OverloadedLabels #-}

module DrawNode ( drawNode ) where

import           Types
import qualified Utils

import qualified DearImGui
import           DearImGui ( ImVec2(..) )
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad.State
import           Data.Function ( (&) )
import qualified Data.Map as Map


drawNode :: Node -> StateT AppState IO ()
drawNode node = do

  appState <- get

  let act = node ^. #activity
  
      pad_Y = 20

      actNamePos  = node ^. #drawPos
      servNamePos = ImVec2 ( x actNamePos ) ( y actNamePos + pad_Y )
      
      cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  -- TODO: Add an "Edit" button and the alternative text input box.
  setCursorPos'' actNamePos
  DearImGui.text $ act ^. #name

  setCursorPos'' servNamePos
  isServComboOpen <- DearImGui.beginCombo ( "##Service Combo For " ++ act ^. #name ) ( act ^. #service )
  case isServComboOpen of
    False -> return ()
    True  -> do
      let drawServSel :: String -> StateT AppState IO ()
          drawServSel serv = do
            isSelected <- DearImGui.selectable serv
            case isSelected of
              False -> return ()
              True  -> return ()

          drawServSels :: StateT AppState IO ()
          drawServSels = mapM_ drawServSel ( appState ^. #appData . #allServiceNames )

      put =<< ( liftIO $ execStateT drawServSels appState )
      DearImGui.endCombo
                        
