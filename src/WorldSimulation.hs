{-# LANGUAGE ScopedTypeVariables #-}

module WorldSimulation
( simulateWorld
, readyPlayer
) where

import Apecs
import SDL

import Control.Monad(when)

import Common
import Components
import Characters

-- Simulate the world, advancing time until the player must act
simulateWorld :: System' ()
simulateWorld = do
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
  cmap (\(Character n h _ _ stats cbStats a) -> Examine $ 
    n ++ ": " ++ show a ++ ", Health: " ++ show h ++ "/" ++ show (maxHealth cbStats))

