{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Components
( AllComps
, Time(..)
, WindowSize(..)
, MessageBit(..)
, MBit(..)
, Messages(..)
, GameState(..)
, Renderer(..)
, Textures(..)
, Fonts(..)
, GameMode(..)
, Tile(..)
, GameMap(..)
, Player(..)
, PlayerReady(..)
, PlayerPath(..)
, Relationships(..)
, Reticule(..)
, Position(..)
, CellRef(..)
, Examine(..)
, Sprite(..)
, FloatingTex(..)
) where

import Apecs
import SDL hiding (Vector, Renderer)
import qualified SDL
import qualified SDL.Font
import qualified Data.HashMap.Strict as HM
import Data.Matrix

import Resources
import Characters

-- Easy type for all non-global, non-player components
type AllComps = (Position, CellRef, Sprite, Character, FloatingTex)

-- Global component, exists outside of entities
-- Used for managing the passage of time
newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

-- This component acts as a flag for when the player can move
newtype PlayerReady = PlayerReady Bool deriving Show
instance Semigroup PlayerReady where (<>) = mappend
instance Monoid PlayerReady where mempty = PlayerReady True
instance Component PlayerReady where type Storage PlayerReady = Global PlayerReady

-- Global store of window size
newtype WindowSize = WindowSize (V2 Int) deriving Show
instance Semigroup WindowSize where (<>) = mappend
instance Monoid WindowSize where mempty = WindowSize (V2 0 0)
instance Component WindowSize where type Storage WindowSize = Global WindowSize

-- Global component used for reporting events
class MessageBit msg where render :: msg -> (String, SDL.Font.Color)
instance MessageBit String where render msg = (msg, V4 255 255 255 255)
instance MessageBit (String, SDL.Font.Color) where render msg = msg
data MBit = forall m. MessageBit m => MBit m 

newtype Messages = Messages [[MBit]]
instance Semigroup Messages where (<>) = mappend
instance Monoid Messages where mempty = Messages []
instance Component Messages where type Storage Messages = Global Messages

-- Global store of renderer
newtype Renderer = Renderer (Maybe SDL.Renderer)
instance Component Renderer where type Storage Renderer = Global Renderer
instance Semigroup Renderer where (<>) = mappend
instance Monoid Renderer where mempty = Renderer Nothing

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
data Tile = Empty | Solid deriving (Show, Eq)
newtype GameMap = GameMap (Matrix Tile)
instance Component GameMap where type Storage GameMap = Global GameMap
instance Semigroup GameMap where (<>) = mappend
instance Monoid GameMap where mempty = GameMap $ fromList 0 0 []

-- Global store of player's pathing
newtype PlayerPath = PlayerPath [V2 Int]
instance Component PlayerPath where type Storage PlayerPath = Global PlayerPath
instance Semigroup PlayerPath where (<>) = mappend
instance Monoid PlayerPath where mempty = PlayerPath []

-- Global store of all relationships in the game
newtype Relationships = Relationships RelationshipTable deriving Show
instance Component Relationships where type Storage Relationships = Global Relationships
instance Semigroup Relationships where (<>) = mappend
instance Monoid Relationships where mempty = Relationships HM.empty

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
newtype Examine = Examine [MBit]
instance Component Examine where type Storage Examine = Map Examine

-- Floating tooltips for combat etc
data FloatingTex = FloatingTex SDL.Texture (V2 Double)
instance Component FloatingTex where type Storage FloatingTex = Map FloatingTex
