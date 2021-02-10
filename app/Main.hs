{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import           Control.Exception ( bracket, bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.Managed ( runManaged, managed, managed_ )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )

import qualified DearImGui
import           DearImGui.OpenGL2 ( openGL2Init, openGL2Shutdown, openGL2NewFrame, openGL2RenderDrawData )
import qualified DearImGui.SDL
import           DearImGui.SDL ( sdl2Shutdown, sdl2NewFrame )
import           DearImGui.SDL.OpenGL ( sdl2InitForOpenGL )
import qualified Graphics.GL
import qualified SDL

import           Foreign.C.Types
import           Foreign.Ptr

main :: IO ()
main = do

  SDL.initializeAll

  imguiWindowSize <- newIORef $ DearImGui.ImVec2 1280 720

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

    liftIO $ mainLoop window imguiWindowSize


mainLoop :: SDL.Window -> IORef DearImGui.ImVec2 -> IO ()
mainLoop window windowSize = do
  -- Process the event loop
  untilNothingM DearImGui.SDL.pollEventWithImGui

  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame window
  DearImGui.newFrame

  -- Set window size.
  SDL.V2 (CInt windowSizeX) (CInt windowSizeY) <- SDL.get $ SDL.windowSize window
  writeIORef windowSize $ DearImGui.ImVec2 (fromIntegral windowSizeX) (fromIntegral windowSizeY)
  DearImGui.setNextWindowSize windowSize DearImGui.ImGuiCond_Always

  -- Add widgets.
  bracket_ ( DearImGui.begin "Hello, ImGui!" ) DearImGui.end do
    -- Add a text widget
    DearImGui.text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    DearImGui.button "Clickety Click" >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

    testRef <- Data.IORef.newIORef "test"
    DearImGui.inputText "Input" testRef 30

  -- Show the ImGui demo window
  DearImGui.showDemoWindow

  -- Render
  Graphics.GL.glClear Graphics.GL.GL_COLOR_BUFFER_BIT

  DearImGui.render
  openGL2RenderDrawData =<< DearImGui.getDrawData

  SDL.glSwapWindow window

  mainLoop window windowSize

  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)
