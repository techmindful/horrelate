{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language RankNTypes #-}

module DrawNode ( drawNode ) where

import           Node
import           Types
import qualified Utils.Utils as Utils

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


-- The pos of the first identifier's type. It's the topmost and the leftmost.
identTypeStartPos node = ImVec2 ( x $ servNamePos node ) ( ( y ( servNamePos node ) ) + pad_Y )
identTypePoses node = zipWith ImVec2
  [ node & identTypeStartPos & x, node & identTypeStartPos & x .. ]
  [ node & identTypeStartPos & y, ( node & identTypeStartPos & y ) + pad_Y .. ]


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

      drawIdents appState =
        liftIO $ execStateT
          ( sequence_ $ zipWith ( drawIdent node ) ( identTypePoses node ) ( Map.toList $ node ^. #activity . #identifiers ) )
          appState

      drawNoEdit = do
        ( liftIO $ execStateT ( sequence_ $ List.map ( fst . snd ) ( Map.toList drawMap ) ) appState ) >>=
          drawIdents >>=
            put


  DearImGui.pushItemWidth 150


  case appState ^. #nodeEdit of
    Nothing ->
      drawNoEdit

    Just nodeEdit -> do
      -- If editing this node
      if nodeEdit ^. #actName == actName then do
        let
          f_PickFunc = \( field, ( f, f_Edit ) ) -> 
              if field == nodeEdit ^. #field then f_Edit else f

          fs = List.map f_PickFunc ( Map.toList drawMap )

        ( liftIO $ execStateT ( sequence_ fs ) appState ) >>=
          drawIdents >>=
            put
      else
        drawNoEdit

  minRef <- liftIO $ newIORef $ ImVec2 0 0
  maxRef <- liftIO $ newIORef $ ImVec2 200 200
  DearImGui.addRect minRef maxRef 4294967295


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


drawIdent :: Node -> ImVec2 -> ( String, String ) -> StateT AppState IO ()
drawIdent node pos ( identType, identVal ) = do

  appState <- get

  let act = node ^. #activity
      actName = act ^. #name

  let identTypePos   = pos
      identTypeWidth = 100
      identValPos    = ImVec2 ( x identTypePos + identTypeWidth ) ( y identTypePos )
      identValWidth  = 150
      editBtnPos     = ImVec2 ( x identValPos + identValWidth ) ( y identTypePos )
      confirmBtnPos  = editBtnPos
      cancelBtnPos   = ImVec2 ( x confirmBtnPos + 60 ) ( y confirmBtnPos )
  
  let cursorPosRef'  = appState ^. #cursorPosRef
      setCursorPos'' = Utils.setCursorPos' cursorPosRef'

  let imGuiIdPostFix = "##for identifier " ++ identType ++ " " ++ identVal ++ " for " ++ actName

  let drawNoEdit_Ident = do
        setCursorPos'' identTypePos
        DearImGui.text identType

        setCursorPos'' identValPos
        DearImGui.text identVal

        setCursorPos'' editBtnPos
        DearImGui.button ( "Edit" ++ imGuiIdPostFix ) >>= \case
          False -> return ()
          True  -> do
            put $ appState & #nodeEdit .~ ( Just $ NodeEdit { actName = actName, field = IdentField identType } )
                           -- Clear previous edit.
                           & #nodeIdentEdit .~ NoEdit

  case appState ^. #nodeEdit of
    Nothing ->
      drawNoEdit_Ident

    Just nodeEdit -> do
      let isEditingThisField =
            ( nodeEdit ^. #actName == actName ) &&
            ( nodeEdit ^. #field   == IdentField identType ) 

      if not isEditingThisField then
        drawNoEdit_Ident
      else do

        -- Draw identifier type combo.

        let typeComboActiveStr = case appState ^. #nodeIdentEdit of
              IdentEdit typeStr _ -> typeStr
              _ -> ""

        setCursorPos'' pos
        DearImGui.pushItemWidth identTypeWidth
        isTypeComboOpen <- DearImGui.beginCombo ( "##Identifier type combo" ++ imGuiIdPostFix ) typeComboActiveStr
        case isTypeComboOpen of
          False -> return ()
          True  -> do
            let
              drawIdentTypeSel :: String -> StateT AppState IO ()
              drawIdentTypeSel identType = do
                isSelected <- DearImGui.selectable identType
                case isSelected of
                  False -> return ()
                  True  -> put $ appState & #nodeIdentEdit .~ IdentEdit identType Nothing

              drawIdentTypeSels :: StateT AppState IO ()
              drawIdentTypeSels = mapM_ drawIdentTypeSel ( Map.keys $ appState ^. #appData . #allIdentifiers )

            put =<< ( liftIO $ execStateT drawIdentTypeSels appState )
            DearImGui.endCombo
        DearImGui.popItemWidth

        -- Draw identifier value combo.
        
        let valComboActiveStr = case appState ^. #nodeIdentEdit of
              IdentEdit _ ( Just valStr ) -> valStr
              _ -> ""

        setCursorPos'' identValPos
        DearImGui.pushItemWidth identValWidth
        isValComboOpen <- DearImGui.beginCombo ( "##Identifier value combo" ++ imGuiIdPostFix ) valComboActiveStr
        case isValComboOpen of
          False -> return ()
          True  -> do
            case appState ^. #nodeIdentEdit of
              IdentEdit identType _ -> do
                let
                  onSel val = put $ appState & #nodeIdentEdit .~ IdentEdit identType ( Just val )

                  maybeVals = Map.lookup identType ( appState ^. #appData . #allIdentifiers )
                  vals = case maybeVals of
                           Nothing -> []
                           Just vals -> vals

                  drawIdentValSel = \val -> drawComboSel val ( onSel val )

                  drawIdentValSels = mapM_ drawIdentValSel vals

                put =<< ( liftIO $ execStateT drawIdentValSels appState )

              _ -> return ()

            DearImGui.endCombo
        DearImGui.popItemWidth

        -- Draw the buttons.

        setCursorPos'' confirmBtnPos
        DearImGui.button ( "Confirm" ++ imGuiIdPostFix ) >>= \case
          False -> return ()
          True  -> do
            let f_UpdateIdentMap = ( Map.insert typeComboActiveStr valComboActiveStr ) . ( Map.delete identType )
                newIdentMap = f_UpdateIdentMap $ node ^. #activity . #identifiers

            put $ appState & #appData . #nodes %~ updateNodes actName ( #activity . #identifiers ) newIdentMap
                           & #nodeEdit .~ Nothing

        setCursorPos'' cancelBtnPos
        DearImGui.button ( "Cancel" ++ imGuiIdPostFix ) >>= \case
          False -> return ()
          True  -> put $ appState & #nodeEdit .~ Nothing


drawComboSel :: String -> StateT AppState IO () -> StateT AppState IO ()
drawComboSel selStr onSel = do
  isSelected <- DearImGui.selectable selStr
  case isSelected of
    False -> return ()
    True  -> onSel


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

