{-# LANGUAGE ScopedTypeVariables #-}

module ActionStep
( actionStep
) where

import Apecs 
import SDL 

import Control.Monad(when)

import Common
import Components
import Characters

-- System called whenever a character performs something
-- This needs to be NON-DESTRUCTIVE and IMPORTANT
-- It happens per character not per turn
actionStep :: System' ()
actionStep = do
  killDeadCharacters
  writeCombatStats
  capHealths

-- Kill any characters with zero health
-- We don't want characters targetting dead people
killDeadCharacters :: System' ()
killDeadCharacters = do
  ls :: [(Character, Not Player, Entity)] <- getAll
  mapM_ (\(c, _, e) -> when (health c <= 0) $ 
    destroy e (Proxy :: Proxy AllComps)) ls

-- Updates combat stats for each character
-- Potions or debuffs must be instant
writeCombatStats :: System' ()
writeCombatStats = cmap (\(c :: Character) -> c {
  combatStats = calculateCombatStats $ stats c })

-- Ensure that no-one's healths are above maximum
-- Nothing should make the character's health go over
capHealths :: System' ()
capHealths = cmap (\(c :: Character) -> c {
  health = min (health c) (maxHealth $ combatStats c)})

