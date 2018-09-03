module Characters
( Character(..)
, Attitude(..)
) where

-- For defining relationships with people
data Attitude = Friendly | Neutral | Aggressive deriving Show

-- A character component contains everything a basic character needs
-- Name: Something to reference in lore
-- Health: Current health for use in combat and death mechanics
-- Max Health: The maximum capacity for the character's health
-- Attitude: How the character will respond to the player's presence
data Character = 
  Character 
    { name :: String
    , health :: Int
    , maxHealth :: Int
    , attitude :: Attitude
    }
