{-# language LambdaCase #-}
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

      actNamePos    = node ^. #drawPos
      actEditBtnPos = ImVec2 ( x actNamePos + 100 ) ( y actNamePos )

      servNamePos = ImVec2 ( x actNamePos ) ( y actNamePos + pad_Y )
      
      cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  setCursorPos'' actEditBtnPos
  DearImGui.button ( "Edit## activity name " ++ act ^. #name ) >>= \case
    False -> return ()
    True  -> put $ appState & #editingActivityName .~ ( Just $ act ^. #name )

  setCursorPos'' actNamePos
  case fmap ( == act ^. #name ) ( appState ^. #editingActivityName ) of
    Just True -> do
      DearImGui.inputText ( "##Editing Activity " ++ act ^. #name ) ( appState ^. #activityNameEditRef ) 128
      return ()

    _ -> DearImGui.text $ act ^. #name

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
                        
