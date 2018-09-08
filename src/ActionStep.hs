{-# LANGUAGE ScopedTypeVariables #-}

module ActionStep
( actionStep
) where

import Apecs 
import SDL 

import Control.Monad(when)

import Components
import Common
import Characters

-- System called whenever the player performs a proper action
actionStep :: System' ()
actionStep = do
  killDeadCharacters
  writeCombatStats
  capHealths
  writeExamines

-- Kill any characters with zero health
killDeadCharacters :: System' ()
killDeadCharacters = do
  ls :: [(Character, Entity)] <- getAll
  mapM_ (\(c, e) -> when (health c <= 0) $ 
    destroy e (Proxy :: Proxy AllComps)) ls

-- Updates combat stats for each character
writeCombatStats :: System' ()
writeCombatStats = cmap (\(c :: Character) -> c {
  combatStats = calculateCombatStats $ stats c })

-- Ensure that no-one's healths are above maximum
capHealths :: System' ()
capHealths = cmap (\(c :: Character) -> c {
  health = min (health c) (maxHealth $ combatStats c)})

-- Place important information into examine messages
writeExamines :: System' ()
writeExamines = 
  cmap (\(Character n h stats cbStats a) -> Examine $ 
    n ++ ": " ++ show a ++ ", Health: " ++ show h ++ "/" ++ show (maxHealth cbStats))

