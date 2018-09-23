{-# LANGUAGE ScopedTypeVariables #-}

module WorldSimulation
( simulateWorld
, readyPlayer
, navigatePlayer
, executePath
, playerActionStep
) where

import Apecs
import SDL.Vect
import qualified SDL

import Data.Matrix
import Data.List(find)
import Control.Monad(when, unless)

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
import Characters
import CharacterActions
import ActionStep
import GameMap
import Draw

-- Simulate the world, advancing time until the player must act
simulateWorld :: System' ()
simulateWorld = do
  snapEntities
  simulateCharacters
  regenHealthAndEnergy
  [(Player, c :: Character, e :: Entity)] <- getAll
  if energy c > 0
    then simulateWorld
    else set global $ PlayerReady True

-- Get the player ready for their turn
-- Run systems that only affect the player
readyPlayer :: System' ()
readyPlayer = do
  set global $ PlayerReady False
  writeExamines
  [(Player, CellRef pos)] <- getAll
  executePath pos

executePath :: V2 Int -> System' ()
executePath pos = do
  PlayerPath path <- get global
  case path of
    [] -> pure ()
    (x : xs) ->
      case vectToDirection (x - pos) of
        Just dir -> do
          set global $ PlayerPath xs
          navigatePlayer dir
        Nothing ->
          set global $ PlayerPath []

-- The player has made their move and is ready to simulate
-- This spends the player's energy
playerActionStep :: Int -> System' ()
playerActionStep cost = do
  when (cost > 0) $ do
    [(Player, c, p)] <- getAll
    set p $ c { energy = cost}
  actionStep
  simulateWorld

-- Easy type synonym
type Comps = (Character, CellRef, Entity)

-- Make all non-player characters act if they have enough energy
simulateCharacters :: System' ()
simulateCharacters = do
  GameMap m <- get global
  ls :: [Comps] <- getAll
  mapM_ (manipulateCharacter m ls) ls

-- Regenerate every character's health and decrease cooldowns
regenHealthAndEnergy :: System' ()
regenHealthAndEnergy = cmapM (\(c :: Character, Position p) -> do
  let allowRegen = regenTimer c <= 0
      hr = healthRegen $ combatStats c
      hrCapped = min (max 0 (maxHealth (combatStats c) - health c)) hr
  when (allowRegen && hrCapped > 0) $ spawnFloatingText (show hrCapped) (V4 0 255 0 255) p
  pure c 
    { health = 
      if allowRegen && hrCapped > 0
        then health c + hrCapped
        else health c
    , regenTimer = 
      if allowRegen
        then 10
        else regenTimer c - 1
    , energy = max 0 $ energy c - energyRegen (combatStats c)})

-- Place important information into examine messages
writeExamines :: System' ()
writeExamines = 
  cmap (\(Character name h e _ stats cbStats f nature _) -> Examine $ 
    name ++ ": " ++ f ++ ", " ++ show nature ++ ", Health: " ++ show h ++ "/" ++ show (maxHealth cbStats))

-- Manipulate each character with respect to the map and other chars
-- This is a BIG function. For now, check attitude and attack the player
manipulateCharacter :: Matrix Tile -> [Comps] -> Comps ->  System' ()
manipulateCharacter gm ls (c, CellRef p, e) =
  unless (energy c > 0) $ 
    case nature c of
      Aggressive ->
        meleeCloseTargets e (map (\(_, cell, ent) -> (cell, ent)) ls) p
      _ -> pure ()

-- Attack entities in range
meleeCloseTargets :: Entity -> [(CellRef, Entity)] -> V2 Int -> System' ()
meleeCloseTargets this ls (V2 x y) = do
  let allDirs = [CellRef $ V2 (x + i) (y + j) | i <- [-1 .. 1], j <- [-1 .. 1]]
      es = foldl (\eList dir -> eList ++ 
        case lookup dir ls of 
          Just e -> if this == e then [] else [e]
          _ -> []) [] allDirs
  unless (null es) $ do
    this `attack` head es
    actionStep

-- Move, swap, or fight in a given direction, standard navigation
navigatePlayer :: Direction -> System' ()
navigatePlayer dir = do
  GameMap m <- get global
  [(Player, CellRef pos, pChar :: Character, p)] <- getAll
  chars :: CharacterList <- getAll
  let dest = pos + directionToVect dir
      action = getNavAction m (dir, dest) chars
  case action of
    Left na ->
      case na of
        Move -> do
          set p $ CellRef dest
          playerActionStep 100
        Swap e c -> do
          set e $ CellRef pos
          set p $ CellRef dest
          postMessage $ "You switch places with " ++ name c ++ "!"
          playerActionStep 100
        Fight e -> do
          p `attack` e
          playerActionStep 0
    Right msg -> 
      postMessage msg

-- Things that can come from navigation
data NavAction = Move | Swap Entity Character | Fight Entity

-- Given a direction, find how to execute the player's intent
getNavAction :: Matrix Tile -> (Direction, V2 Int) -> CharacterList -> Either NavAction String
getNavAction g (dir, dest) cs = 
  case tile of
    Nothing -> Right "Hmm.. You can't find a way to move there."
    Just tile -> 
      if tile == Empty
        then case charOnSpace of
          Nothing -> Left Move
          Just (c, _, e) -> Right "TEMPORARILY DISABLED"
           --case attitude c of
           --  Aggressive -> Left $ Fight e
           --  Friendly -> Left $ Swap e c
           --  Neutral -> Right $ "Oof! You bumped into " ++ name c ++ "!"
        else Right "Ouch! You bumped into a wall!"
  where tile = getTile g dest
        chk (Character {}, CellRef p, _) = dest == p
        charOnSpace = find chk cs

