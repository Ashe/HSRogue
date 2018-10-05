module Characters
( CharacterInfo(..)
, Stats(..)
, CombatStats(..)
, Faction
, Attitude(..)
, Nature(..)
, RelationshipTable
, defaultRelationships
, calculateCombatStats
, initialStats
, initialCombatStats
) where

import Apecs (Entity)
import Data.HashMap.Strict as HM

-- A character component contains everything a basic character needs
-- Name: Something to reference in lore
-- Health: Current health for use in combat and death mechanics
-- Regen Timer: Ticks until the next regen occurs
-- Energy: Accumilator for when the character can do something
-- Stats: Character stats
-- Combat stats: A description of the character's current state based on stats and items
-- Attitude: How the character will respond to the player's presence
data CharacterInfo = 
  CharacterInfo
    { name :: String
    , health :: Int
    , regenTimer :: Int
    , energy :: Int
    , stats :: Stats
    , combatStats :: CombatStats
    , faction :: Faction
    , nature :: Nature
    , target :: Maybe Entity
    } deriving Show

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
    , visionRange :: Int
    } deriving Show

-- For defining relationships with people
-- Faction: Who the character aligns with
-- Attitude: For defining how a faction reacts to another
-- Nature: How an individual acts (whether they fight or not)
type Faction = String
data Attitude = Friendly | Neutral | Hostile deriving (Show, Eq)
data Nature = Passive | Defensive | Aggressive deriving (Show, Eq)
type RelationshipTable = HM.HashMap Faction (HM.HashMap Faction Attitude)

-- Default relationship table
defaultRelationships :: RelationshipTable
defaultRelationships = fromList
    [ ("Edgelords", fromList
        [ ("Player", Hostile)
        ])
    ]

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
    , visionRange = 3
    }

-- Default stats
initialStats :: Stats
initialStats = Stats 10 10 10 10

-- Default combat stats
initialCombatStats :: CombatStats
initialCombatStats = calculateCombatStats initialStats
