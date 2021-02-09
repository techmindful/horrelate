{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import           Control.Exception ( bracket, bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.Managed ( runManaged, managed, managed_ )
import qualified DearImGui
import           DearImGui.OpenGL2 ( openGL2Init, openGL2Shutdown, openGL2NewFrame, openGL2RenderDrawData )
import qualified DearImGui.SDL
import           DearImGui.SDL ( sdl2Shutdown, sdl2NewFrame )
import           DearImGui.SDL.OpenGL ( sdl2InitForOpenGL )
import qualified Graphics.GL
import qualified SDL

main :: IO ()
main = do

  SDL.initializeAll

  runManaged do
    -- Create a window using SDL. As we're using OpenGL, we need to enable OpenGL too.
    w <- do
      let title = "Hello, Dear ImGui!"
      let config = SDL.defaultWindow { SDL.windowGraphicsContext = SDL.OpenGLContext SDL.defaultOpenGL }
      managed $ bracket ( SDL.createWindow title config ) SDL.destroyWindow

    -- Create an OpenGL context
    glContext <- managed $ bracket ( SDL.glCreateContext w ) SDL.glDeleteContext

    -- Create an ImGui context
    _ <- managed $ bracket DearImGui.createContext DearImGui.destroyContext

    -- Initialize ImGui's SDL2 backend
    _ <- managed_ $ bracket_ ( sdl2InitForOpenGL w glContext) sdl2Shutdown

    -- Initialize ImGui's OpenGL backend
    _ <- managed_ $ bracket_ openGL2Init openGL2Shutdown

    liftIO $ mainLoop w


mainLoop :: SDL.Window -> IO ()
mainLoop w = do
  -- Process the event loop
  untilNothingM DearImGui.SDL.pollEventWithImGui

  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame w
  DearImGui.newFrame

  -- Build the GUI
  bracket_ ( DearImGui.begin "Hello, ImGui!" ) DearImGui.end do
    -- Add a text widget
    DearImGui.text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    DearImGui.button "Clickety Click" >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

  -- Show the ImGui demo window
  DearImGui.showDemoWindow

  -- Render
  Graphics.GL.glClear Graphics.GL.GL_COLOR_BUFFER_BIT

  DearImGui.render
  openGL2RenderDrawData =<< DearImGui.getDrawData

  SDL.glSwapWindow w

  mainLoop w

  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)
