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
, playerPos
, playerCellRef
, playerSpeed
) where

import Apecs
import Linear

import Components

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''Player, ''Position, ''CellRef, ''Sprite] 

-- Easy type synonym for systems
type System' a = System World a

-- Directions
data Direction = 
  Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving Show

-- Conversion to int vector for calculations
directionToVect :: Direction -> V2 Int
directionToVect dir = case dir of
                        Up -> V2 0 1
                        UpRight -> V2 1 1
                        Common.Right -> V2 1 0
                        DownRight -> V2 1 (-1)
                        Common.Down -> V2 0 (-1)
                        DownLeft -> V2 (-1) (-1)
                        Common.Left -> V2 (-1) 0
                        UpLeft -> V2 (-1) 1

worldScale :: Double
worldScale = 30

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

playerSpeed :: Int
playerSpeed = 1
