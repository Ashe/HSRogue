{-# LANGUAGE TypeFamilies #-}

module Components
( Time(..)
, Messages(..)
, Player(..)
, Position(..)
, CellRef(..)
, Textures(..)
, Sprite(..)
, GameMap(..)
) where

import Apecs
import Apecs.Stores
import SDL hiding (Vector)
import qualified Data.HashMap as HM
import Data.Vector

import ImageLoad
import GameMap

-- Global component, exists outside of entities
-- Used for managing the passage of time
newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

-- Global component used for debugging and reporting
newtype Messages = Messages [String] deriving Show
instance Semigroup Messages where (<>) = mappend
instance Monoid Messages where mempty = Messages []
instance Component Messages where type Storage Messages = Global Messages

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
newtype Textures = Textures TextureMap
instance Component Textures where type Storage Textures = Global Textures
instance Semigroup Textures where (<>) = mappend
instance Monoid Textures where mempty = Textures HM.empty

-- Used to store the texture coordinates of a sprite
data Sprite = Sprite String (Rectangle Int)
instance Component Sprite where type Storage Sprite = Map Sprite

-- Global store of the current game map
newtype GameMap = GameMap Grid
instance Component GameMap where type Storage GameMap = Global GameMap
instance Semigroup GameMap where (<>) = mappend
instance Monoid GameMap where mempty = GameMap empty
