{-# language BlockArguments #-}
{-# language OverloadedLabels #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main ( main ) where

import           Types
import           MainLoop ( mainLoop )

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
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Control.Monad.Managed ( runManaged, managed, managed_ )
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LzByteStrC8
import           Data.Function ( (&) )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.Map.Strict as Map
import qualified Data.List.Safe as Safe


main :: IO ()
main = do

  appDataRaw <- LzByteStrC8.readFile "test-save.json"
  let maybeAppData :: Maybe AppData = decode appDataRaw
  maybe ( return () ) ( putStrLn . show ) maybeAppData

  SDL.initializeAll

  let
    imguiWindowWidth  = 1280
    imguiWindowHeight = 720

  let paddingXY = ImVec2 20 20

  let cmdInputPos = ImVec2 ( x paddingXY ) ( imguiWindowHeight - y paddingXY - 20 )  -- Guessing inputText height to be 20 for now.

  imguiWindowSizeRef <- newIORef $ ImVec2 imguiWindowWidth imguiWindowHeight
  imguiWindowPosRef  <- newIORef $ ImVec2 0 0
  cmdInputPosRef     <- newIORef $ cmdInputPos
  cmdInputRef        <- newIORef $ ""

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    window <- do
      let title = "Horrelate"
      let config = SDL.defaultWindow {
          SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL
          , SDL.windowInitialSize = SDL.V2 1280 720
          , SDL.windowResizable = True
          }
      managed $ bracket ( SDL.createWindow title config ) SDL.destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket ( SDL.glCreateContext window ) SDL.glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket DearImGui.createContext DearImGui.destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ ( sdl2InitForOpenGL window glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    case maybeAppData of
      Just appData -> do

        newCursorPosRef <- liftIO $ newIORef $ ImVec2 0 0
        newServiceNameEditRef <- liftIO $ newIORef ""
        newIdentifierValueEditRef <- liftIO $ newIORef ""
        newNodeActNameEditRef <- liftIO $ newIORef ""

        let initAppState = AppState {} & #appData .~ appData

                                       & #editingService .~ Nothing
                                       & #serviceNameEditRef .~ newServiceNameEditRef

                                       & #identifierTypeSel .~ Nothing
                                       & #editingIdentifierValue .~ Nothing
                                       & #identifierValueEditRef .~ newIdentifierValueEditRef

                                       & #nodeEdit .~ Nothing

                                       & #nodeActNameEditRef .~ newNodeActNameEditRef

                                       & #cursorPosRef .~ newCursorPosRef

        liftIO $ evalStateT ( mainLoop window imguiWindowPosRef imguiWindowSizeRef cmdInputPosRef cmdInputRef paddingXY ) initAppState

      _ ->
        liftIO $ putStrLn "Failed to load appData."
        

