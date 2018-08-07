{-# LANGUAGE TypeFamilies #-}

module Components
( Player(..)
, Time(..)
, Position(..)
, CellRef(..)
, Sprite(..)
) where

import Apecs
import Apecs.Stores
import Linear
import Graphics.Gloss

-- Global component, exists outside of entities
-- Used for managing the passage of time
newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

-- Unique component, either one or none exists
data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

-- Used to store the position of game entities
newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

-- Used to store the cell reference of an entity
newtype CellRef = CellRef (V2 Int) deriving Show
instance Component CellRef where type Storage CellRef = Map CellRef

-- Used to store the graphics of entities
newtype Sprite = Sprite (Maybe Picture)
instance Component Sprite where type Storage Sprite = Map Sprite
