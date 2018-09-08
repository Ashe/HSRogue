{-# LANGUAGE ScopedTypeVariables #-}

module CharacterActions
( attack
) where

import Apecs

import Common
import Characters

-- Make one character attack another
attack :: Entity -> Entity -> System' ()
attack a v = do
  ac :: Character <- get a
  vc :: Character <- get v
  let damage = getDamage ac vc
      vc' = dealDamage damage vc
  set v vc'
  if health vc' > 0 then do
    postMessage $ name ac ++ " attacks " ++ name vc' ++ " for " ++ show damage ++ " damage!"
    postMessage $ name vc' ++ " has " ++ show (health vc') ++ " health left!"
  else
    postMessage $ name ac ++ " kills " ++ name vc' ++ " with " ++ show (health vc') ++ " overkill damage!"

-- Get the damage to be dealt
getDamage :: Character -> Character -> Int
getDamage atk def = max (dam - def) 0
  where dam = strength $ stats atk
        def = 0

-- Deal simple damage to enemy health
dealDamage :: Int -> Character -> Character
dealDamage d c = c { health = health c - d}
