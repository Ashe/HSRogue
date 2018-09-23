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

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
import GameMap
import Characters
import CharacterActions
import WorldSimulation
import ActionStep

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
      (state :: GameState) <- get global
      case state of 
        Game m -> ingameMouseAction m b (fromIntegral <$> p)
        Interface -> pure ()
    Released -> pure ()

-- Easy method of getting a tile from mouse input
convertToTileCoords :: Matrix Tile -> V2 Int -> Maybe (V2 Int)
convertToTileCoords m (V2 x y) = const p <$> getTile m p
  where p = let (V2 w h) = tileSize in V2 (x `div` w) (y `div` h)

-- Do something in game in response to the mouse
ingameMouseAction :: GameMode -> MouseButton -> V2 Int -> System' ()
ingameMouseAction m b p = do
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
          examinePos tp
          cmap (\(Reticule _, CellRef _) -> CellRef tp)
        Nothing -> pure ()
    _ -> pure ()

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

-- For the handling keyboard events only
handleKeyEvent :: KeyboardEventData -> System' ()
handleKeyEvent ev = do
  (state :: GameState) <- get global
  PlayerReady r <- get global
  let code = keysymKeycode $ keyboardEventKeysym ev
  when r $ case keyboardEventKeyMotion ev of
    Pressed ->
      case state of
        Game mode -> gameAction mode code
        Interface -> postMessage "Interface state not implemented yet"
    Released -> pure ()

-- Use GameState to determine the context of input
-- Use context specific bindings to ascertain intent
data GameIntent
  = Navigate Direction
  | ToggleLook
  | Wait
  deriving (Read, Show, Eq)

-- Initial bindings for intents
defaultGameIntents :: [(Keycode, GameIntent)]
defaultGameIntents = 
  -- Navigation
  [ (KeycodeUp , Navigate C.Up)
  , (KeycodeK, Navigate C.Up)
  , (KeycodeY, Navigate C.UpLeft)
  , (KeycodeLeft , Navigate C.Left)
  , (KeycodeH, Navigate C.Left)
  , (KeycodeB, Navigate C.DownLeft)
  , (KeycodeDown , Navigate C.Down)
  , (KeycodeJ, Navigate C.Down)
  , (KeycodeN, Navigate C.DownRight)
  , (KeycodeRight , Navigate C.Right)
  , (KeycodeL, Navigate C.Right)
  , (KeycodeU, Navigate C.UpRight)

  -- Other functions
  , (KeycodeSemicolon, ToggleLook)
  , (KeycodeW, Wait)
  ]

-- For keyboard events that  take place in the game
gameAction :: GameMode -> Keycode -> System' ()
gameAction mode k = 
  let intents = lookup k defaultGameIntents in
    case mode of
      Standard -> 
        case intents of
          Just (Navigate dir) -> navigatePlayer dir
          Just ToggleLook -> toggleLook mode
          Just Wait -> do
            postMessage "You wait.."
            playerActionStep 100
          _ -> pure ()
      Look -> 
        case intents of
          Just (Navigate dir) -> moveReticule dir
          Just ToggleLook -> toggleLook mode
          _ -> pure ()

-- Turn look mode on to examine entities in the area
toggleLook :: GameMode -> System' ()
toggleLook m = do
  let isLook = m == Look
  modify global (\(a :: GameState) -> 
    if isLook then Game Standard else Game Look)
  ls :: [(Reticule, Entity)] <- getAll
  [(Player, CellRef p)] <- getAll 
  let r = (Reticule $ not isLook, Position (V2 0 0), CellRef p)
  if not $ null ls
    then set (snd $ head ls) r
    else void $ newEntity r
  es :: [(CellRef, Examine)] <- getAll
  unless isLook $ 
    case lookup (CellRef p) es of 
      Just (Examine msg) -> postMessage msg
      _ -> pure ()

-- Move the reticule for looking or aiming purposes
moveReticule :: Direction -> System' ()
moveReticule dir = 
  cmapM (\(Reticule _, CellRef p@(V2 x y)) -> do
    ls :: [(CellRef, Examine)] <- getAll
    let pos = p + directionToVect dir
    examinePos pos
    pure $ CellRef pos)
