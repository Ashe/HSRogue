{-# LANGUAGE TypeFamilies #-}

module Components
( Player(..)
, Time(..)
, Position(..)
, CellRef(..)
, TextureComp(..)
, Sprite(..)
, GameMapComp(..)
) where

import Apecs
import Apecs.Stores
import SDL
import qualified Data.HashMap as HM

import ImageLoad
import GameMap

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
newtype TextureComp = TextureComp Textures
instance Component TextureComp where type Storage TextureComp = Global TextureComp
instance Semigroup TextureComp where (<>) = mappend
instance Monoid TextureComp where mempty = mempty

-- Used to store the texture coordinates of a sprite
data Sprite = Sprite String (Rectangle Int)
instance Component Sprite where type Storage Sprite = Map Sprite

-- Global store of the current game map
newtype GameMapComp = GameMapComp GameMap
instance Component GameMapComp where type Storage GameMapComp = Global GameMapComp
instance Semigroup GameMapComp where (<>) = mappend
instance Monoid GameMapComp where mempty = mempty
