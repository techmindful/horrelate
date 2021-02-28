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
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map as Map


drawNode :: Node -> StateT AppState IO ()
drawNode node = do

  appState <- get

  let act = node ^. #activity
  
      pad_Y = 20

      actNamePos = node ^. #drawPos
      actEditBtnPos = ImVec2 ( x actNamePos + 170 ) ( y actNamePos )
      actConfirmBtnPos = actEditBtnPos
      actCancelBtnPos  = ImVec2 ( x actConfirmBtnPos + 60 ) ( y actConfirmBtnPos )

      servNamePos = ImVec2 ( x actNamePos ) ( y actNamePos + pad_Y )
      
      cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'


  DearImGui.pushItemWidth 150


  case fmap ( == act ^. #name ) ( appState ^. #editingActivityName ) of
    Just True -> do
      setCursorPos'' actNamePos
      DearImGui.inputText ( "##Editing Activity " ++ act ^. #name ) ( appState ^. #activityNameEditRef ) 128

      setCursorPos'' actConfirmBtnPos
      DearImGui.button ( "Confirm## activity name " ++ act ^. #name ) >>= \case
        False -> return ()
        True  -> do
          newActName <- liftIO $ readIORef $ appState ^. #activityNameEditRef

          let f_UpdateNodes :: [ Node ] -> [ Node ]
              f_UpdateNodes nodes =
                map ( \node ->
                  if node ^. #activity . #name == act ^. #name then
                    node & #activity . #name .~ newActName
                  else node
                ) nodes

          put $ appState & #appData . #nodes %~ f_UpdateNodes

      setCursorPos'' actCancelBtnPos
      DearImGui.button ( "Cancel## activity name " ++ act ^. #name ) >>= \case
        False -> return ()
        True  -> put $ appState & #editingActivityName .~ Nothing

    _ -> do
      setCursorPos'' actNamePos
      DearImGui.text $ act ^. #name

      setCursorPos'' actEditBtnPos
      DearImGui.button ( "Edit## activity name " ++ act ^. #name ) >>= \case
        False -> return ()
        True  -> do
          liftIO $ writeIORef ( appState ^. #activityNameEditRef ) ""
          put $ appState & #editingActivityName .~ ( Just $ act ^. #name )

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


  DearImGui.popItemWidth
                        
