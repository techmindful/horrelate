{-# language BlockArguments #-}
{-# language FlexibleContexts #-}
{-# language LambdaCase #-}
{-# language OverloadedLabels #-}
{-# language ScopedTypeVariables #-}

module MainLoop ( mainLoop ) where

import           Consts
import           IdentifiersPanel ( drawIdentifiersPanel )
import           ServicePanel     ( drawServicePanel )
import           ParseCmd ( parseCmd )
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
import           Control.Lens ( (^.), (.~), (%~) )
import           Control.Monad.IO.Class
import           Control.Monad.State
import           Data.IORef ( IORef, newIORef, readIORef, writeIORef )
import           Data.Function ( (&) )
import qualified Data.Map as Map

import           Foreign.C.Types
import           Foreign.Ptr


mainLoop
  :: SDL.Window
  -> ImGuiWindowPosRef
  -> ImGuiWindowSizeRef
  -> CmdInputPosRef
  -> CmdInputRef
  -> PaddingXY
  -> StateT AppState IO ()
mainLoop
  window
  windowPosRef
  windowSizeRef
  cmdInputPosRef
  cmdInputRef
  paddingXY
  = do

  events <- DearImGui.SDL.pollEventsWithImGui

  cmdInput <- liftIO $ readIORef cmdInputRef

  let
    eventPayloads = SDL.eventPayload <$> events

    shouldSubmitCmd = any ( isKeyHit SDL.KeycodeReturn ) eventPayloads

    eitherCmd :: Either String Command
    eitherCmd = parseCmd cmdInput
  
    -- App should quit if there's an SDL.QuitEvent,
    -- Or user submitted a Quit command.
    shouldQuit = any ( == SDL.QuitEvent ) eventPayloads
              || ( eitherCmd == Right Quit && shouldSubmitCmd )

  -- Process user command.
  if shouldSubmitCmd then do
    case parseCmd cmdInput of
      Left errStr ->
        liftIO $ putStrLn errStr

      Right Add -> do
        let newActivity = Activity {
          reg = Registration {
            email = "Test"
          , phoneNum = "123"
          }
        }
        let newNode = Node {
          drawPos  = ImVec2 100 100
        , activity = newActivity
        }
        return ()
        --newNode <- liftIO $ newIORef "Test email"
        --modify (\nodes -> newNode : nodes)

      Right Quit ->
        liftIO $ putStrLn "Quitting"
  else
    return ()


  appState <- get


  -- Tell ImGui we're starting a new frame
  openGL2NewFrame
  sdl2NewFrame window
  DearImGui.newFrame

  -- Resize and place ImGui window to fit SDL window.
  SDL.V2 (CInt windowWidth) (CInt windowHeight) <- SDL.get $ SDL.windowSize window
  liftIO $ writeIORef windowSizeRef $ ImVec2 (fromIntegral windowWidth) (fromIntegral windowHeight)
  DearImGui.setNextWindowSize windowSizeRef DearImGui.ImGuiCond_Always
  DearImGui.setNextWindowPos windowPosRef DearImGui.ImGuiCond_Once Nothing

  -- Build window, add widgets.
  appState' <- liftIO $ bracket_ ( DearImGui.begin "Main Window" ) DearImGui.end do

    -- Draw cmd input.
    let cmdInputPos = ImVec2 ( x paddingXY ) ( fromIntegral windowHeight - y paddingXY - 20 )
    writeIORef cmdInputPosRef cmdInputPos
    DearImGui.setCursorPos cmdInputPosRef
    DearImGui.inputText "Input" cmdInputRef 512

    let drawNode :: Node -> StateT AppState IO ()
        drawNode node = do

          appState <- get

          let act = node ^. #activity
          
              pad_Y = 20

              actNamePos  = node ^. #drawPos
              servNamePos = ImVec2 ( x actNamePos ) ( y actNamePos + pad_Y )
              
              cursorPosRef'  = appState ^. #cursorPosRef
              setCursorPos'' = Utils.setCursorPos' cursorPosRef'

          setCursorPos'' actNamePos
          DearImGui.text $ act ^. #name

          setCursorPos'' servNamePos
          DearImGui.text $ act ^. #service

        drawNodes :: StateT AppState IO ()
        drawNodes = mapM_ drawNode ( appState ^. #appData . #nodes )

    execStateT drawServicePanel appState >>=
      execStateT drawIdentifiersPanel >>=
        execStateT drawNodes

  -- Show the ImGui demo window
  DearImGui.showDemoWindow

  -- Render
  Graphics.GL.glClear Graphics.GL.GL_COLOR_BUFFER_BIT

  DearImGui.render
  openGL2RenderDrawData =<< DearImGui.getDrawData

  SDL.glSwapWindow window

  if shouldQuit then
    return ()
  else do
    liftIO $ evalStateT (mainLoop window windowPosRef windowSizeRef cmdInputPosRef cmdInputRef paddingXY) appState'

  where
    untilNothingM m = m >>= maybe (return ()) (\_ -> untilNothingM m)


isKeyHit :: SDL.Keycode -> SDL.EventPayload -> Bool
isKeyHit keyCode evPayload =
  case evPayload of
    SDL.KeyboardEvent kbEvData ->
      -- True if keycode matches, and key was released.
      -- TODO: Find out why this seems to return true when key is held.
      if ( SDL.keysymKeycode $ SDL.keyboardEventKeysym kbEvData ) == keyCode
        &&                ( SDL.keyboardEventKeyMotion kbEvData ) == SDL.Released then True
      else False

    _ -> False
