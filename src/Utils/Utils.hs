{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language ScopedTypeVariables #-}

module Utils.Utils where

import           Types

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


setCursorPos' :: MonadIO m => IORef ImVec2 -> ImVec2 -> m ()
setCursorPos' posRef pos = liftIO do
  writeIORef posRef pos
  DearImGui.setCursorPos posRef

