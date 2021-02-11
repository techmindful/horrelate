{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}

module Main ( main ) where

import           Control.Exception ( bracket, bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.Managed ( runManaged, managed, managed_ )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )

import qualified DearImGui
import           DearImGui ( ImVec2(..) )
import           DearImGui.OpenGL2 ( openGL2Init, openGL2Shutdown, openGL2NewFrame, openGL2RenderDrawData )
import qualified DearImGui.SDL
import           DearImGui.SDL ( sdl2Shutdown, sdl2NewFrame )
import           DearImGui.SDL.OpenGL ( sdl2InitForOpenGL )
import qualified Graphics.GL
import qualified SDL

import           Foreign.C.Types
import           Foreign.Ptr


type ImGuiWindowPosRef  = IORef ImVec2
type ImGuiWindowSizeRef = IORef ImVec2
type CmdInputPosRef     = IORef ImVec2
type CmdInputRef        = IORef String

type PaddingXY = ImVec2

main :: IO ()
main = do

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

    liftIO $ mainLoop window imguiWindowPosRef imguiWindowSizeRef cmdInputPosRef cmdInputRef paddingXY


mainLoop
  :: SDL.Window
  -> ImGuiWindowPosRef
  -> ImGuiWindowSizeRef
  -> CmdInputPosRef
  -> CmdInputRef
  -> PaddingXY
  -> IO ()
mainLoop
  window
  windowPosRef
  windowSizeRef
  cmdInputPosRef
  cmdInputRef
  paddingXY
  = do
  -- Process the event loop
  untilNothingM DearImGui.SDL.pollEventWithImGui

  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame window
  DearImGui.newFrame

  -- Resize and place ImGui window to fit SDL window.
  SDL.V2 (CInt windowWidth) (CInt windowHeight) <- SDL.get $ SDL.windowSize window
  writeIORef windowSizeRef $ ImVec2 (fromIntegral windowWidth) (fromIntegral windowHeight)
  DearImGui.setNextWindowSize windowSizeRef DearImGui.ImGuiCond_Always
  DearImGui.setNextWindowPos windowPosRef DearImGui.ImGuiCond_Once Nothing

  -- Build window, add widgets.
  bracket_ ( DearImGui.begin "Main Window" ) DearImGui.end do
    -- Add a text widget
    DearImGui.text "Hello, ImGui!"

    -- Add a button widget, and call 'putStrLn' when it's clicked
    DearImGui.button "Clickety Click" >>= \case
      False -> return ()
      True  -> putStrLn "Ow!"

    -- Draw cmd input.
    let cmdInputPos = ImVec2 ( x paddingXY ) ( fromIntegral windowHeight - y paddingXY - 20 )
    writeIORef cmdInputPosRef cmdInputPos
    DearImGui.setCursorPos cmdInputPosRef
    DearImGui.inputText "Input" cmdInputRef 30

  -- Show the ImGui demo window
  DearImGui.showDemoWindow

  -- Render
  Graphics.GL.glClear Graphics.GL.GL_COLOR_BUFFER_BIT

  DearImGui.render
  openGL2RenderDrawData =<< DearImGui.getDrawData

  SDL.glSwapWindow window

  mainLoop window windowPosRef windowSizeRef cmdInputPosRef cmdInputRef paddingXY

  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)
