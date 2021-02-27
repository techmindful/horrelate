{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language ScopedTypeVariables #-}

module ServicePanel ( drawServicePanel ) where

import           Consts
import           Types
import qualified Utils

import qualified DearImGui
import           DearImGui ( ImVec2(..) )
import           DearImGui.OpenGL2 ( openGL2Init, openGL2Shutdown, openGL2NewFrame, openGL2RenderDrawData )
import qualified DearImGui.SDL
import           DearImGui.SDL ( sdl2Shutdown, sdl2NewFrame )
import           DearImGui.SDL.OpenGL ( sdl2InitForOpenGL )
import qualified Graphics.GL
import qualified SDL
import qualified SDL.Event as SDL
import qualified SDL.Input as SDL

import           Control.Lens ( (%~) )
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Function ( (&) )
import           Data.Generics.Internal.VL ( (^.), (.~) )
import           Data.Generics.Product.Fields ( field )


servicePanelPos  = ImVec2 720 mainWindowHeadingOffset
servicePanelSize = ImVec2 560 300


drawServicePanel :: StateT AppState IO ()
drawServicePanel = do

  appState <- get

  let cursorPosRef' = appState & cursorPosRef

  let posY_List :: [ Float ] = map fromIntegral [ x | x <- [ 0, 20 .. ] ]

  appState' <- liftIO do

    wsRef <- newIORef $ servicePanelSize

    Utils.setCursorPos' cursorPosRef' servicePanelPos

    DearImGui.beginChildOfSize "Service Panel" wsRef
    appState' <- execStateT ( sequence_ $ zipWith3 drawServiceName ( appState ^. #appData . #allServiceNames ) [0..] posY_List ) appState
    DearImGui.endChild

    return appState'

  put appState'


drawServiceName :: String -> Int -> Float -> StateT AppState IO ()
drawServiceName name indexInList posY = do

  appState <- get

  let cursorPosRef' = appState & cursorPosRef

  let textPos       = ImVec2 0 posY
      editButtonPos = ImVec2 380 posY
      delButtonPos  = ImVec2 460 posY

      inputBoxPos       = textPos
      confirmButtonPos  = editButtonPos
      cancelButtonPos   = delButtonPos

  -- Did user click "edit" on this entry?
  case fmap ( == name ) $ appState & editingService of
    Just True -> do
      -- Draw input box.
      Utils.setCursorPos' cursorPosRef' inputBoxPos
      DearImGui.pushItemWidth 380
      DearImGui.inputText "##Service Name Edit" ( appState & serviceNameEditRef ) 128
      DearImGui.popItemWidth

      Utils.setCursorPos' cursorPosRef' confirmButtonPos
      DearImGui.button ( "Confirm##" ++ show indexInList ) >>= \case
        True -> do
          newName <- liftIO $ readIORef $ appState & serviceNameEditRef
          put $ appState & #appData . #allServiceNames %~ map (\oldName -> if oldName == name then newName else oldName)
                         & #editingService  .~ Nothing
          --put $ appState {
          --  allServiceNames = map (\name' -> if name' == name then newName else name') ( appState & allServiceNames )
          --, editingService  = Nothing
          --}

        False -> return ()

      Utils.setCursorPos' cursorPosRef' cancelButtonPos
      DearImGui.button ( "Cancel##" ++ show indexInList ) >>= \case
        True -> put $ appState { editingService = Nothing }
        False -> return ()

    _ -> do
      Utils.setCursorPos' cursorPosRef' textPos  
      DearImGui.text name

      Utils.setCursorPos' cursorPosRef' editButtonPos
      DearImGui.button ( "Edit##" ++ show indexInList ) >>= \case
        True -> do
          liftIO $ writeIORef ( appState & serviceNameEditRef ) ""
          put $ appState { editingService = Just name }

        False -> return ()

      Utils.setCursorPos' cursorPosRef' delButtonPos
      DearImGui.button ( "Del##" ++ show indexInList ) >>= \case
        True ->
          put $ appState & #appData . #allServiceNames %~ filter ( /= name )

        False -> return ()

