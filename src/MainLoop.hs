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

    --shouldSubmitCmd = any ( isKeyHit SDL.KeycodeReturn ) eventPayloads

    shouldQuit = any ( == SDL.QuitEvent ) eventPayloads


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

    let drawNode :: Node -> StateT AppState IO ()
        drawNode node = do

          appState <- get

          let act = node ^. #activity
          
              pad_Y = 20

              actNamePos  = node ^. #drawPos
              servNamePos = ImVec2 ( x actNamePos ) ( y actNamePos + pad_Y )
              
              cursorPosRef'  = appState ^. #cursorPosRef
              setCursorPos'' = Utils.setCursorPos' cursorPosRef'

          -- TODO: Add an "Edit" button and the alternative text input box.
          setCursorPos'' actNamePos
          DearImGui.text $ act ^. #name

          setCursorPos'' servNamePos
          isServComboOpen <- DearImGui.beginCombo ( "##Service Combo For " ++ act ^. #name ) ( act ^. #service )
          case isServComboOpen of
            False -> return ()
            True  -> do
              let drawServSel :: String -> StateT AppState IO ()
                  drawServSel serv = do
                    isSelected <- DearImGui.selectable serv
                    case isSelected of
                      False -> return ()
                      True  -> return ()

                  drawServSels :: StateT AppState IO ()
                  drawServSels = mapM_ drawServSel ( appState ^. #appData . #allServiceNames )

              put =<< ( liftIO $ execStateT drawServSels appState )
              DearImGui.endCombo
                        

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
