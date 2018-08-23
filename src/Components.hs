{-# LANGUAGE TypeFamilies #-}

module Components
( Player(..)
, Time(..)
, Position(..)
, CellRef(..)
, Textures(..)
, Sprite(..)
) where

import Apecs
import Apecs.Stores
import SDL
import qualified Data.HashMap as HM

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

-- Global store of all textures
newtype Textures = Textures (HM.Map String Texture)
instance Component Textures where type Storage Textures = Global Textures
instance Semigroup Textures where (<>) = mappend
instance Monoid Textures where mempty = Textures HM.empty

-- Used to store the texture coordinates of a sprite
data Sprite = Sprite String (Rectangle Int)
instance Component Sprite where type Storage Sprite = Map Sprite
