{-# LANGUAGE ScopedTypeVariables #-}

module HandleGameEvents
( gameAction
, gameActionWithMouse
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
gameAction :: GameMode -> Keycode -> System' ()
gameAction mode k = 
  let intent = lookup k defaultGameIntents in
    case mode of
      Standard -> 
        case intent of
          Just (Navigate dir) -> navigatePlayer dir
          Just ToggleLook -> do
            toggleLook mode
            [(Player, Examine msg)] <- getAll
            postMessage msg
          Just Wait -> do
            postMessage [MBit "You wait.."]
            playerActionStep 100
          Just PauseGame -> openPauseMenu
          _ -> pure ()
      Look -> 
        case intent of
          Just (Navigate dir) -> moveReticule dir
          Just ToggleLook -> toggleLook mode
          Just PauseGame -> openPauseMenu
          _ -> pure ()

-- Do something in game in response to the mouse
gameActionWithMouse :: GameMode -> MouseButton -> V2 Int -> System' ()
gameActionWithMouse m b p = do
  GameMap mp <- get global
  let tile = convertToTileCoords mp p
  case b of
    ButtonLeft -> 
      case m of
        Standard -> 
          case tile of
            Just tp -> pathfindPlayer mp tp
            _ -> pure ()
        Look -> toggleLook m
    ButtonRight -> 
      case tile of
        Just tp -> do
          when (m == Standard) $ toggleLook m
          examinePos tp
          cmap (\(Reticule _, CellRef _) -> CellRef tp)
        Nothing -> pure ()
    _ -> pure ()

-- Use GameState to determine the context of input
-- Use context specific bindings to ascertain intent
data GameIntent
  = Navigate Direction
  | ToggleLook
  | Wait
  | PauseGame
  deriving (Read, Show, Eq)

-- Initial bindings for intents
defaultGameIntents :: [(Keycode, GameIntent)]
defaultGameIntents = 
  -- Navigation
  [ (KeycodeUp , Navigate T.Up)
  , (KeycodeK, Navigate T.Up)
  , (KeycodeY, Navigate T.UpLeft)
  , (KeycodeLeft , Navigate T.Left)
  , (KeycodeH, Navigate T.Left)
  , (KeycodeB, Navigate T.DownLeft)
  , (KeycodeDown , Navigate T.Down)
  , (KeycodeJ, Navigate T.Down)
  , (KeycodeN, Navigate T.DownRight)
  , (KeycodeRight , Navigate T.Right)
  , (KeycodeL, Navigate T.Right)
  , (KeycodeU, Navigate T.UpRight)

  -- Menus
  , (KeycodeEscape, PauseGame)

  -- Other functions
  , (KeycodeSemicolon, ToggleLook)
  , (KeycodeW, Wait)
  ]

-- Turn look mode on to examine entities in the area
toggleLook :: GameMode -> System' ()
toggleLook m = do
  let isLook = m == Look
  modify global (\(a :: GameState) -> 
    GameState $ if isLook then Game Standard else Game Look)
  ls :: [(Reticule, Entity)] <- getAll
  [(Player, CellRef p)] <- getAll 
  let r = (Reticule $ not isLook, Position (V2 0 0), CellRef p)
  if not $ null ls
    then set (snd $ head ls) r
    else void $ newEntity r

-- Move the reticule for looking or aiming purposes
moveReticule :: Direction -> System' ()
moveReticule dir = 
  cmapM (\(Reticule _, CellRef p@(V2 x y)) -> do
    ls :: [(CellRef, Examine)] <- getAll
    let pos = p + directionToVect dir
    examinePos pos
    pure $ CellRef pos)

-- Attempt to move the player to specified location
pathfindPlayer :: Matrix Tile  -> V2 Int -> System' ()
pathfindPlayer m dest = do
  PlayerReady r <- get global
  [(Player, CellRef p)] <- getAll
  case pathfind m p dest of
    Just path -> do
      set global $ PlayerPath path
      when r $ executePlayerPath p
    _ -> pure ()

-- Pause the game
openPauseMenu :: System' ()
openPauseMenu = do
  let col :: SDL.Font.Color = V4 100 100 255 255
  postMessage [MBit ("Pausing Game.", col)]
  set global $ GameState $ Interface PauseScreen
