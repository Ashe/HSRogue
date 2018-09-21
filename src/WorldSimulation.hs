{-# LANGUAGE ScopedTypeVariables #-}

module WorldSimulation
( simulateWorld
, readyPlayer
) where

import Apecs
import SDL.Vect
import qualified SDL

import Data.Matrix
import Control.Monad(when, unless)

import Common
import Components
import Characters
import CharacterActions
import ActionStep
import GameMap

-- Simulate the world, advancing time until the player must act
simulateWorld :: System' ()
simulateWorld = do
  snapEntities
  simulateCharacters
  regenHealthAndEnergy
  [(Player, c :: Character, e :: Entity)] <- getAll
  if energy c > 0
    then simulateWorld
    else readyPlayer

-- Get the player ready for their turn
-- Run systems that only affect the player
readyPlayer :: System' ()
readyPlayer = do
  writeExamines
  pure ()

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
  cmap (\(Character n h e _ stats cbStats a) -> Examine $ 
    n ++ ": " ++ show a ++ ", Health: " ++ show h ++ "/" ++ show (maxHealth cbStats) ++ " Energy: " ++ show e)

-- Manipulate each character with respect to the map and other chars
-- This is a BIG function. For now, check attitude and attack the player
manipulateCharacter :: Matrix Tile -> [Comps] -> Comps ->  System' ()
manipulateCharacter gm ls (c, CellRef p, e) =
  unless (energy c > 0) $ 
    case attitude c of
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
