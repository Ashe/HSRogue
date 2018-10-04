{-# LANGUAGE ScopedTypeVariables #-}

module HandleInterfaceEvents
( interfaceAction
, interfaceActionWithMouse
) where

import Apecs hiding (Map)
import SDL hiding (get)
import SDL.Font

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

-- For keyboard events that  take place in the game
interfaceAction :: InterfaceMode -> Keycode -> System' ()
interfaceAction mode k = 
  let intent = lookup k defaultInterfaceIntents in
    case intent of
      Just Exit -> do
        let col :: SDL.Font.Color = V4 100 100 255 255
        postMessage [MBit ("Resuming Game.", col)]
        set global $ GameState $ Game Standard
      _ -> pure ()

-- Do something in game in response to the mouse
interfaceActionWithMouse :: InterfaceMode -> MouseButton -> V2 Int -> System' ()
interfaceActionWithMouse mode b p = pure ()

-- Use context specific bindings to ascertain intent
data InterfaceIntent
  = Navigate Direction
  | Exit
  | Interact
  deriving (Read, Show, Eq)

-- Initial bindings for intents
defaultInterfaceIntents :: [(Keycode, InterfaceIntent)]
defaultInterfaceIntents =
  -- Navigation
  [ (KeycodeUp , Navigate T.Up)
  , (KeycodeK, Navigate T.Up)
  , (KeycodeLeft , Navigate T.Left)
  , (KeycodeH, Navigate T.Left)
  , (KeycodeDown , Navigate T.Down)
  , (KeycodeJ, Navigate T.Down)
  , (KeycodeRight , Navigate T.Right)
  , (KeycodeL, Navigate T.Right)

  -- Other functions
  , (KeycodeEscape, Exit)
  , (KeycodeSpace, Interact)
  ]
