module Characters
( Character(..)
, Stats(..)
, CombatStats(..)
, Attitude(..)
, calculateCombatStats
, initialStats
, initialCombatStats
) where

-- A character component contains everything a basic character needs
-- Name: Something to reference in lore
-- Health: Current health for use in combat and death mechanics
-- Max Health: The maximum capacity for the character's health
-- Attitude: How the character will respond to the player's presence
data Character = 
  Character
    { name :: String
    , health :: Int
    , stats :: Stats
    , combatStats :: CombatStats
    , attitude :: Attitude
    }

-- Generic character stats
-- Dictates character abilities and combat stats
data Stats = 
  Stats
    { strength :: Int
    , agility :: Int
    , toughness :: Int
    } deriving Show

-- Combat stats
-- Dictates effectiveness in battle
data CombatStats = 
  CombatStats
    { maxHealth :: Int
    , criticalChance :: Int
    , criticalMult :: Double
    , bonusDamage :: Int
    } deriving Show

-- For defining relationships with people
data Attitude = Friendly | Neutral | Aggressive deriving Show

-- Calculate combat stats based on stats, equipment, skills and auras
calculateCombatStats :: Stats -> CombatStats
calculateCombatStats s = 
  CombatStats 
    { maxHealth = 20 * toughness s
    , criticalChance = agility s
    , criticalMult = 1.5
    , bonusDamage = 0
    }

-- Default stats
initialStats :: Stats
initialStats = Stats 10 10 10

-- Default combat stats
initialCombatStats :: CombatStats
initialCombatStats = calculateCombatStats initialStats
