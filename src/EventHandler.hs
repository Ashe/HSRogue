{-# LANGUAGE ScopedTypeVariables #-}

module EventHandler
( handlePayload
) where

import Apecs hiding (Map)
import SDL hiding (get)

import Control.Monad(when, unless, void)
import Control.Monad.IO.Class
import Data.Maybe(isNothing)
import Data.List(find)

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
        Interface -> postMessage "Interface state not implemented yet"
    Released -> pure ()

-- The player has made their move and is ready to simulate
-- This spends the player's energy
playerActionStep :: Entity -> Int -> System' ()
playerActionStep p cost = do
  cmap (\(Player, c :: Character) -> c { energy = energy c + cost})
  actionStep
  simulateWorld

-- Use GameState to determine the context of input
-- Use context specific bindings to ascertain intent
data GameIntent
  = Navigate Direction
  | ToggleLook
  deriving (Read, Show, Eq)

-- Initial bindings for intents
defaultGameIntents :: [(Keycode, GameIntent)]
defaultGameIntents = 
  -- Navigation
  [ (KeycodeUp , Navigate C.Up)
  , (KeycodeK, Navigate C.Up)
  , (KeycodeLeft , Navigate C.Left)
  , (KeycodeH, Navigate C.Left)
  , (KeycodeDown , Navigate C.Down)
  , (KeycodeJ, Navigate C.Down)
  , (KeycodeRight , Navigate C.Right)
  , (KeycodeL, Navigate C.Right)

  -- Nav modes
  , (KeycodeSemicolon, ToggleLook)
  ]

-- For keyboard events that  take place in the game
gameAction :: GameMode -> Keycode -> System' ()
gameAction mode k = 
  let intents = lookup k defaultGameIntents in
    case mode of
      Standard -> 
        case intents of
          Just (Navigate dir) -> navigate dir
          Just ToggleLook -> toggleLook mode
          _ -> pure ()
      Look -> 
        case intents of
          Just (Navigate dir) -> moveReticule dir
          Just ToggleLook -> toggleLook mode
          _ -> pure ()

-- Things that can come from navigation
data NavAction = Move | Swap Entity Character | Fight Entity

-- Move, swap, or fight in a given direction, standard navigation
navigate :: Direction -> System' ()
navigate dir = do
  GameMap m <- get global
  [(Player, CellRef pos, pChar :: Character, p)] <- getAll
  chars :: CharacterList <- getAll
  let dest = pos + directionToVect dir
      action = getNavAction m (dir, dest) chars
  case action of
    Left na -> do
      case na of
        Move -> 
          set p $ CellRef dest
        Swap e c -> do
          set e $ CellRef pos
          set p $ CellRef dest
          postMessage $ "You switch places with " ++ name c ++ "!"
        Fight e -> 
          p `attack` e
      playerActionStep p 100
    Right msg -> 
      postMessage msg

-- Given a direction, find how to execute the player's intent
getNavAction :: Grid -> (Direction, V2 Int) -> CharacterList -> Either NavAction String
getNavAction g (dir, dest) cs = 
  case tile of
    Nothing -> Right "Hmm.. You can't find a way to move there."
    Just tile -> 
      if tile == Empty
        then case charOnSpace of
          Nothing -> Left Move
          Just (c, _, e) -> 
            case attitude c of
              Aggressive -> Left $ Fight e
              Friendly -> Left $ Swap e c
              Neutral -> Right $ "Oof! You bumped into " ++ name c ++ "!"
        else Right "Ouch! You bumped into a wall!"
  where tile = getTile g dest
        chk (Character {}, CellRef p, _) = dest == p
        charOnSpace = find chk cs

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
    case lookup (CellRef pos) ls of 
      Just (Examine msg) -> postMessage msg
      _ -> pure ()
    pure $ CellRef pos)
