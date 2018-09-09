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
-- Regen Timer: Ticks until the next regen occurs
-- Energy: Accumilator for when the character can do something
-- Stats: Character stats
-- Combat stats: A description of the character's current state based on stats and items
-- Attitude: How the character will respond to the player's presence
data Character = 
  Character
    { name :: String
    , health :: Int
    , regenTimer :: Int
    , energy :: Int
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
    , speed :: Int
    } deriving Show

-- Combat stats
-- Dictates effectiveness in battle
data CombatStats = 
  CombatStats
    { healthRegen :: Int
    , energyRegen :: Int
    , maxHealth :: Int
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
    { healthRegen = 1
    , energyRegen = speed s
    , maxHealth = 20 * toughness s
    , criticalChance = agility s
    , criticalMult = 1.5
    , bonusDamage = 0
    }

-- Default stats
initialStats :: Stats
initialStats = Stats 10 10 10 10

-- Default combat stats
initialCombatStats :: CombatStats
initialCombatStats = calculateCombatStats initialStats
