{-# language BlockArguments #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language ScopedTypeVariables #-}

module Main ( main ) where

import           Control.Exception ( bracket, bracket_ )
import           Control.Monad.IO.Class
import           Control.Monad.Managed ( runManaged, managed, managed_ )
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import qualified Data.List.Safe as Safe
import           Data.Function ( (&) )
import           Control.Error.Util ( note )

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

import           Foreign.C.Types
import           Foreign.Ptr


data Activity = Activity {
  reg :: Registration
}

data Registration = Registration {
    email    :: Email
  , phoneNum :: PhoneNum
  , name     :: Name
  , address  :: Address
}

newtype Email     = Email String
newtype PhoneNum  = PhoneNum Int
data Name         = Name {
    firstName :: String
  , midName   :: String
  , lastName  :: String
}
data Address      = Address {
    street  :: String
  , apt     :: String
  , city    :: String
  , country :: String 
}

data Command
  = Add String
  | Quit

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

  events <- DearImGui.SDL.pollEventsWithImGui

  cmdInput <- readIORef cmdInputRef

  let isKeyHit :: SDL.Keycode -> SDL.EventPayload -> Bool
      isKeyHit keyCode evPayload =
        case evPayload of
          SDL.KeyboardEvent kbEvData ->
            -- True if keycode matches, and key was released.
            -- TODO: Find out why this seems to return true when key is held.
            if ( SDL.keysymKeycode $ SDL.keyboardEventKeysym kbEvData ) == keyCode
              &&                ( SDL.keyboardEventKeyMotion kbEvData ) == SDL.Released then True
            else False

          _ -> False

  let eventPayloads = SDL.eventPayload <$> events
  
  let shouldQuit        = any ( == SDL.QuitEvent ) eventPayloads
      shouldSubmitCmd   = any ( isKeyHit SDL.KeycodeReturn ) eventPayloads

  let parseCmd :: String -> Either String Command
      parseCmd str =
        let cmdTokens = words cmdInput
            maybeVerb = Safe.head cmdTokens
            maybeNoun :: Maybe String = ( Safe.!! ) cmdTokens 1
        in do
          verb <- maybeVerb & note "Error: Empty input."
          case verb of
            "add" -> do
              noun <- maybeNoun & note "Error: \"add\" requires a noun."
              return $ Add noun

            "quit" ->
              return Quit

            _ ->
              Left "Error: Unknown command."

  if shouldSubmitCmd then do
    case parseCmd cmdInput of
      Left errStr ->
        putStrLn errStr

      Right ( Add str ) ->
        putStrLn $ "Adding " ++ str 

      Right Quit ->
        putStrLn "Quitting"
  else
    return ()

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

  if shouldQuit then
    return ()
  else
    mainLoop window windowPosRef windowSizeRef cmdInputPosRef cmdInputRef paddingXY

  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)
