{-# LANGUAGE ScopedTypeVariables #-}

module EventHandler
( handlePayload
) where

import Apecs hiding (Map)
import SDL hiding (get)

import Data.Map(Map, insert, empty, lookup)

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
import GameMap

-- Handle the entire event payload
handlePayload :: [EventPayload] -> System' ()
handlePayload = foldl (\_ ev -> handleEvent ev) (pure ()) 
  
-- The main event handler function for dealing with keypresses
handleEvent :: EventPayload -> System' ()
handleEvent (KeyboardEvent ev) = handleKeyEvent ev
handleEvent _ = pure ()

-- For the handling keyboard events only
handleKeyEvent :: KeyboardEventData -> System' ()
handleKeyEvent ev = do
  (state :: GameState) <- get global
  let code = keysymKeycode $ keyboardEventKeysym ev
  case keyboardEventKeyMotion ev of
    Pressed -> 
      case state of
        Game mode -> gameAction mode code
        Interface -> pure ()
    _ -> pure ()

-- Use GameState to determine the context of input
-- Use context specific bindings to ascertain intent
data GameIntent
  = Navigate Direction
  | Look
  deriving (Read, Show, Eq)

-- Initial bindings for intents
defaultGameIntents :: Map Keycode GameIntent
defaultGameIntents = foldl (\m (k, v) -> insert k v m) empty
  [ (KeycodeUp , Navigate C.Up)
  , (KeycodeK, Navigate C.Up)
  , (KeycodeLeft , Navigate C.Left)
  , (KeycodeH, Navigate C.Left)
  , (KeycodeDown , Navigate C.Down)
  , (KeycodeJ, Navigate C.Down)
  , (KeycodeRight , Navigate C.Right)
  , (KeycodeL, Navigate C.Right)
  ]

-- For keyboard events that  take place in the game
gameAction :: GameMode -> Keycode -> System' ()
gameAction mode k = case mode of
  Standard -> movePlayer intentDir
  _ -> pure ()
  where intent = Data.Map.lookup k defaultGameIntents
        intentDir = case intent of
                      Just (Navigate dir) -> Just dir
                      _ -> Nothing

-- Move the player in a direction using move speed
movePlayer :: Maybe Direction -> System' ()
movePlayer (Just dir) = do
  GameMap m <- get global
  postMessage ("You move " ++ show dir ++ ".")
  let (V2 i j) = directionToVect dir
  cmap $ \(Player, pos@(CellRef (V2 x y))) -> 
    let dest = V2 (x + i * playerSpeed) (y + j * playerSpeed) in
        if checkDir m dest then CellRef dest else pos
movePlayer _ = pure ()

-- Check to see if the move is valid
checkDir :: Grid -> V2 Int -> Bool
checkDir g dest = 
  case tile of
    Just tile -> tile == Empty
    _ -> False
  where tile = getTile g dest

-- Find a direction of movement from scancode
findDir :: Scancode -> Maybe Direction
findDir ScancodeW = Just C.Up
findDir ScancodeUp = Just C.Up
findDir ScancodeD = Just C.Right
findDir ScancodeRight = Just C.Right
findDir ScancodeS = Just C.Down
findDir ScancodeDown = Just C.Down
findDir ScancodeA = Just C.Left
findDir ScancodeLeft = Just C.Left
findDir _ = Nothing
