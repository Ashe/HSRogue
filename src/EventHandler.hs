{-# LANGUAGE ScopedTypeVariables #-}

module EventHandler
( handlePayload
) where

import Apecs hiding (Map)
import SDL hiding (get)

import Control.Monad(when, unless, void, forM_)
import Control.Monad.IO.Class
import Data.Maybe(isNothing, isJust)
import Data.List(find)
import Data.Matrix

import Types as T
import Common
import Components
import GameMap
import Characters
import CharacterActions
import WorldSimulation
import ActionStep

import HandleGameEvents
import HandleInterfaceEvents

-- Handle the entire event payload
handlePayload :: [EventPayload] -> System' ()
handlePayload = mapM_ handleEvent 
  
-- The main event handler function for dealing with keypresses
handleEvent :: EventPayload -> System' ()
handleEvent (MouseButtonEvent ev) = handleMouseEvent ev
handleEvent (KeyboardEvent ev) = handleKeyEvent ev
handleEvent (WindowResizedEvent ev) = handleResizeEvent ev
handleEvent _ = pure ()

-- Handling of the window changing size
handleResizeEvent :: WindowResizedEventData -> System' ()
handleResizeEvent (WindowResizedEventData _ s) = 
  set global $ WindowSize $ fromIntegral <$> s

-- For handling mouse events
handleMouseEvent :: MouseButtonEventData -> System' ()
handleMouseEvent (MouseButtonEventData _ bm _ b _ (P p)) =
  case bm of
    Pressed -> do
      GameState state <- get global
      let mousepos = fromIntegral <$> p
      case state of 
        Game m -> gameActionWithMouse m b mousepos
        Interface mode -> interfaceActionWithMouse mode b mousepos
    Released -> pure ()

-- For the handling keyboard events only
handleKeyEvent :: KeyboardEventData -> System' ()
handleKeyEvent ev = do
  GameState state <- get global
  PlayerReady r <- get global
  let code = keysymKeycode $ keyboardEventKeysym ev
  when r $ case keyboardEventKeyMotion ev of
    Pressed ->
      case state of
        Game mode -> gameAction mode code
        Interface mode -> interfaceAction mode code
    Released -> pure ()

-- Use GameState to determine the context of input
-- Use context specific bindings to ascertain intent
data GameIntent
  = Navigate Direction
  | ToggleLook
  | Wait
  deriving (Read, Show, Eq)
