{-# LANGUAGE ScopedTypeVariables #-}

module EventHandler
( handlePayload
) where

import Apecs hiding (Map)
import SDL hiding (get)

import Data.Map(Map, insert, empty, lookup)
import Control.Monad(when)
import Data.Maybe(isNothing)
import Data.List(find)

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
import GameMap
import Characters

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
movePlayer Nothing = pure ()
movePlayer (Just dir) = do
  GameMap m <- get global
  [((Player, CellRef (V2 x y)), pId)] <- getAll
  chars :: CharacterList <- getAll
  let (V2 i j) = directionToVect dir
      dest = V2 (x + i * playerSpeed) (y + j * playerSpeed)
      valid = checkDir m dest chars
  case valid of
    Left success -> 
      when success $ do
      modify pId (\(CellRef _) -> CellRef dest)
      postMessage $ "You move " ++ show dir ++ "."
    Right msg -> 
      postMessage msg

-- Check to see if the move is valid
checkDir :: Grid -> V2 Int -> CharacterList -> Either Bool String
checkDir g dest cs = 
  case tile of
    Nothing -> Right "Hmm.. You can't find a way to move there."
    Just tile -> 
      if tile == Empty
        then case charOnSpace of
          Nothing -> Left True
          Just ((c, _), _) -> Right $ "Oof! You bumped into " ++ name c ++ "!"
        else Right "Ouch! You bumped into a wall!"
  where tile = getTile g dest
        chk ((Character {}, CellRef p), _) = dest == p
        charOnSpace = find chk cs

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
