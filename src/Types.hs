{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}

module Types
( State(..)
, GameMode(..)
, MessageBit(..)
, MBit(..)
, FPS
, TexResource
, TextureMap
, FontResource
, FontMap
, Direction(..)
) where

import Apecs
import SDL hiding (Vector, Renderer)
import qualified SDL
import qualified SDL.Font
import Data.HashMap.Strict

-- Data for managing flow of the game
data GameMode = Standard | Look deriving (Show, Eq)
data State = Game GameMode | Interface deriving (Show, Eq)

-- Class for managing text within the game
class MessageBit msg where render :: msg -> (String, SDL.Font.Color)
instance MessageBit String where render msg = (msg, V4 255 255 255 255)
instance MessageBit (String, SDL.Font.Color) where render msg = msg
data MBit = forall m. MessageBit m => MBit m 

-- Easy FPS 
type FPS = Int

-- Types for creating textures
type TexResource = (String, Texture)
type TextureMap = HashMap String Texture

-- Types for creating fonts
type FontResource = (String, SDL.Font.Font)
type FontMap = HashMap String SDL.Font.Font

-- Types for Directions
data Direction = 
  Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving (Read, Show, Eq, Ord)

