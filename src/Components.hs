{-# LANGUAGE TypeFamilies #-}

module Components
( AllComps
, Time(..)
, Messages(..)
, GameState(..)
, Textures(..)
, Fonts(..)
, GameMode(..)
, GameMap(..)
, Player(..)
, Reticule(..)
, Position(..)
, CellRef(..)
, Examine(..)
, Sprite(..)
) where

import Apecs
import SDL hiding (Vector)
import qualified Data.HashMap as HM
import Data.Vector

import Resources
import GameMap
import Characters

-- Easy type for all non-global, non-player components
type AllComps = (Position, CellRef, Sprite, Character)

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

-- Global store of all textures
newtype Textures = Textures TextureMap
instance Component Textures where type Storage Textures = Global Textures
instance Semigroup Textures where (<>) = mappend
instance Monoid Textures where mempty = Textures HM.empty

-- Global store of all fonts
newtype Fonts = Fonts FontMap
instance Component Fonts where type Storage Fonts = Global Fonts
instance Semigroup Fonts where (<>) = mappend
instance Monoid Fonts where mempty = Fonts HM.empty

-- Global component used for changing gamestates
data GameMode = Standard | Look deriving (Show, Eq)
data GameState = Game GameMode | Interface deriving (Show, Eq)
instance Semigroup GameState where (<>) = mappend
instance Monoid GameState where mempty = Game Standard
instance Component GameState where type Storage GameState = Global GameState

-- Global store of the current game map
newtype GameMap = GameMap Grid
instance Component GameMap where type Storage GameMap = Global GameMap
instance Semigroup GameMap where (<>) = mappend
instance Monoid GameMap where mempty = GameMap empty

-- Unique component, either one or none exists
data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

-- Unique Component for showing where the player is looking
newtype Reticule = Reticule Bool deriving Show
instance Component Reticule where type Storage Reticule = Unique Reticule

-- Position of game entities
newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

-- Cell reference of an entity
newtype CellRef = CellRef (V2 Int) deriving (Show, Eq)
instance Component CellRef where type Storage CellRef = Map CellRef

-- Texture coordinates of a sprite
data Sprite = Sprite String (Rectangle Int)
instance Component Sprite where type Storage Sprite = Map Sprite

-- Character elements of the player and NPCs
instance Component Character where type Storage Character = Map Character

-- Descriptions of entities when looking
newtype Examine = Examine String deriving Show
instance Component Examine where type Storage Examine = Map Examine
