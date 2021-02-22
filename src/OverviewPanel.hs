{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module OverviewPanel ( drawOverviewPanel ) where

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

import           Control.Exception ( bracket, bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Function ( (&) )

import           Foreign.C.Types
import           Foreign.Ptr


overviewPanelPos  = ImVec2 720 mainWindowHeadingOffset
overviewPanelSize = ImVec2 560 300


drawOverviewPanel :: StateT AppState IO ()
drawOverviewPanel = do

  appState <- get

  let cursorPosRef' = appState & cursorPosRef

  let posY_List :: [ Float ] = map fromIntegral [ x | x <- [ 0, 20 .. ] ]

  liftIO do

    wsRef <- newIORef $ overviewPanelSize

    writeIORef cursorPosRef' overviewPanelPos
    DearImGui.setCursorPos cursorPosRef'

    DearImGui.beginChildOfSize "Overview Panel" wsRef

    mapM_ (\stateT -> runStateT stateT appState) $ zipWith3 drawActivityName ( appState & allActivityNames ) [0..] posY_List

    DearImGui.endChild


drawActivityName :: String -> Int -> Float -> StateT AppState IO ()
drawActivityName name indexInList posY = do

  appState <- get

  let cursorPosRef' = appState & cursorPosRef

  let textDrawPos       = ImVec2 0 posY
      editButtonDrawPos = ImVec2 80 posY
      delButtonDrawPos  = ImVec2 160 posY

  Utils.setCursorPos' cursorPosRef' textDrawPos  
  case fmap ( == indexInList ) $ appState & editingActivity of
    Just True ->
      DearImGui.text "EDITING"
    _ ->
      DearImGui.text name

  Utils.setCursorPos' cursorPosRef' editButtonDrawPos
  DearImGui.button "Edit" >>= \case
    True -> modify (\appState' -> appState' { editingActivity = Just indexInList })
    False -> return ()

  Utils.setCursorPos' cursorPosRef' delButtonDrawPos
  DearImGui.button "Del" >>= \case
    True -> return ()
    False -> return()

