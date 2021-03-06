{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language RankNTypes #-}

module DrawNode ( drawNode ) where

import           Types
import qualified Utils

import qualified DearImGui
import           DearImGui ( ImVec2(..) )
import           Control.Lens ( Lens',  (^.), (.~), (%~) )
import           Control.Monad.State
import           Data.Function ( (&) )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.List as List
import qualified Data.Map  as Map


pad_Y = 20


actNamePos :: Node -> ImVec2
actNamePos node  = node ^. #drawPos
actEditBtnPos node = ImVec2 ( x ( actNamePos node ) + 170 ) ( y $ actNamePos node )
actConfirmBtnPos = actEditBtnPos
actCancelBtnPos node  = ImVec2 ( x ( actConfirmBtnPos node ) + 60 ) ( y $ actConfirmBtnPos node )

servNamePos node = ImVec2 ( x $ actNamePos node ) ( ( y ( actNamePos node ) ) + pad_Y )
servEditBtnPos node = ImVec2 ( x ( servNamePos node ) + 170 ) ( y $ servNamePos node )
servConfirmBtnPos = servEditBtnPos
servCancelBtnPos node = ImVec2 ( x ( servConfirmBtnPos node ) + 60 ) ( y $ servConfirmBtnPos node )


drawNode :: Node -> StateT AppState IO ()
drawNode node = do

  appState <- get

  let act = node ^. #activity
      actName = act ^. #name
  
  let drawMap_Partial = Map.fromList
        [ ( ActField,  ( drawAct,  drawAct_Edit  ) )
        , ( ServField, ( drawServ, drawServ_Edit ) )
        ]

      drawMap = Map.map ( \( f, f_Edit ) -> ( f node, f_Edit node ) ) drawMap_Partial

      drawNoEdit = liftIO $ execStateT ( sequence_ $ List.map ( fst . snd ) ( Map.toList drawMap ) ) appState


  DearImGui.pushItemWidth 150


  case appState ^. #nodeEdit of
    Nothing ->
      put =<< drawNoEdit

    Just nodeEdit -> do
      -- If editing this node
      if nodeEdit ^. #actName == actName then do
        let
          f_PickFunc = \( field, ( f, f_Edit ) ) -> 
              if field == nodeEdit ^. #field then f_Edit else f

          fs = List.map f_PickFunc ( Map.toList drawMap )

        put =<< ( liftIO $ execStateT ( sequence_ fs ) appState )
      else
        put =<< drawNoEdit


  DearImGui.popItemWidth


drawAct :: Node -> StateT AppState IO ()
drawAct node = do

  appState <- get

  let actName = node ^. #activity . #name
 
  let cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  setCursorPos'' $ actNamePos node
  DearImGui.text $ actName

  setCursorPos'' $ actEditBtnPos node
  DearImGui.button ( "Edit## activity name " ++ actName ) >>= \case
    False -> return ()
    True  -> do
      liftIO $ writeIORef ( appState ^. #nodeActNameEditRef ) ""
      put $ appState & #nodeEdit .~ ( Just $ NodeEdit { actName = actName, field = ActField } )


drawAct_Edit :: Node -> StateT AppState IO ()
drawAct_Edit node = do

  appState <- get

  let actName = node ^. #activity . #name

  let cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  setCursorPos'' $ actNamePos node
  DearImGui.inputText ( "##Editing Activity " ++ actName ) ( appState ^. #nodeActNameEditRef ) 128

  setCursorPos'' $ actConfirmBtnPos node
  DearImGui.button ( "Confirm## activity name " ++ actName ) >>= \case
    False -> return ()
    True  -> do
      newActName <- liftIO $ readIORef $ appState ^. #nodeActNameEditRef
      put $ appState & #appData . #nodes %~ updateNodes actName ( #activity . #name ) newActName

  setCursorPos'' $ actCancelBtnPos node
  DearImGui.button ( "Cancel## activity name " ++ actName ) >>= \case
    False -> return ()
    True  -> put $ appState & #nodeEdit .~ Nothing


drawServ :: Node -> StateT AppState IO ()
drawServ node = do
  
  appState <- get

  let act = node ^. #activity
      actName = act ^. #name
      service = act ^. #service

  let cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  setCursorPos'' $ servNamePos node
  DearImGui.text service

  setCursorPos'' $ servEditBtnPos node
  DearImGui.button( "Edit## service for " ++ actName ) >>= \case
    False -> return ()
    True  -> do
      put $ appState & #nodeServEdit .~ ""
      appState' <- get  -- TODO: How to avoid writing this?
      put $ appState' & #nodeEdit .~ ( Just $ NodeEdit { actName = actName, field = ServField } )


drawServ_Edit :: Node -> StateT AppState IO ()
drawServ_Edit node = do

  appState <- get

  let actName = node ^. #activity . #name
  
  let cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  setCursorPos'' $ servNamePos node
  isServComboOpen <- DearImGui.beginCombo ( "##Service Combo For " ++ actName ) ( appState ^. #nodeServEdit )
  case isServComboOpen of
    False -> return ()
    True  -> do
      let drawServSel :: String -> StateT AppState IO ()
          drawServSel serv = do
            isSelected <- DearImGui.selectable serv
            case isSelected of
              False -> return ()
              True  -> put $ appState & #nodeServEdit .~ serv

          drawServSels :: StateT AppState IO ()
          drawServSels = mapM_ drawServSel ( appState ^. #appData . #allServiceNames )

      put =<< ( liftIO $ execStateT drawServSels appState )
      DearImGui.endCombo

  setCursorPos'' $ servConfirmBtnPos node
  DearImGui.button ( "Confirm## service for " ++ actName ) >>= \case
    False -> return ()
    True  -> do
      put $ appState & #appData . #nodes %~ updateNodes actName ( #activity . #service ) ( appState ^. #nodeServEdit )
      appState' <- get  -- TODO: How to avoid writing this?
      put $ appState' & #nodeEdit .~ Nothing

  setCursorPos'' $ servCancelBtnPos node
  DearImGui.button ( "Cancel## service for " ++ actName ) >>= \case
    False -> return ()
    True  -> put $ appState & #nodeEdit .~ Nothing


updateNodes :: String -> Lens' Node a -> a -> [ Node ] -> [ Node ]
updateNodes actName lens newVal nodes =
  let
    updateNode = \node ->
      if node ^. #activity . #name == actName then
        node & lens .~ newVal
      else
        node
  in
  map updateNode nodes

