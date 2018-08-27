{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common
( World(..)
, initWorld
, System'
, Direction(..)
, worldScale
, directionToVect
, toCIntRect
, toCIntV2
, playerPos
, playerCellRef
, playerSpeed
, tileSize
) where

import Apecs
import SDL
import Foreign.C

import Components

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
-- makeWorld "World" [''Time, ''Player, ''Position, ''CellRef, ''Sprite] 
makeWorld "World" [''Time, ''Textures, ''Player, ''Position, ''CellRef, ''Sprite] 

-- Easy type synonym for systems
type System' a = System World a

-- Directions
data Direction = 
  Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving Show

-- Conversion from Direction to Int V2
directionToVect :: Direction -> V2 Int
directionToVect Common.Up = V2 0 (-1)
directionToVect UpRight = V2 1 (-1)
directionToVect Common.Right = V2 1 0
directionToVect DownRight = V2 1 1
directionToVect Common.Down = V2 0 1
directionToVect DownLeft = V2 (-1) 1
directionToVect Common.Left = V2 (-1) 0
directionToVect UpLeft = V2 (-1) (-1)
directionToVect _ = V2 0 0

-- Conversion from Int Rectangle to CInt Rectangle
toCIntRect :: Rectangle Int -> Rectangle CInt
toCIntRect (Rectangle (P (V2 x y)) (V2 i j)) = 
  Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral i) (fromIntegral j))

toCIntV2 :: V2 Double -> V2 CInt
toCIntV2 (V2 x y) = V2 (round x) (round y)

worldScale :: Double
worldScale = 32

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

playerSpeed :: Int
playerSpeed = 1

tileSize :: V2 CInt
tileSize = V2 32 32
