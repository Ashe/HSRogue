{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common
( World(..)
, initWorld
, System'
, Direction(..)
, CharacterList
, postMessage
, printMessages
, clearMessages
, spawnFloatingText
, floatTooltips
, directionToVect
, toCIntRect
, toCIntV2
, worldScale
, playerPos
, playerCellRef
, tileSize
, tileSize'
) where

import Apecs
import SDL hiding (get, Vector)
import SDL.Font
import Foreign.C
import Data.HashMap as HM
import Data.Text(Text, pack)
import Control.Monad(void)
import Control.Arrow((***))

import Data.Vector (ifoldl)

import Components
import Resources
import GameMap
import Characters

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''Messages, ''GameState, ''Textures, ''Fonts, ''GameMap, 
  ''Player, ''Reticule, ''Position, ''CellRef, ''Sprite, ''Character, ''Examine,
  ''FloatingText] 

-- Easy type synonym for systems
type System' a = System World a

-- Types for Directions
data Direction = 
  Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving (Read, Show, Eq, Ord)

-- Easy way of getting all non-player entities
type CharacterList = [(Character, CellRef, Entity)]

-- Post a new message
postMessage :: String -> System' ()
postMessage [] = pure ()
postMessage m = modify global (\(Messages msgs) -> Messages $ m : msgs)

-- Print messages into console
printMessages :: System' (IO ())
printMessages = do
  Messages msgs <- get global
  pure $ foldl (\io m ->io <> putStrLn m) mempty $ reverse msgs

-- Flush any messages
clearMessages :: System' ()
clearMessages = modify global (\(Messages _) -> Messages [])

-- Spawn a floating tooltip
spawnFloatingText :: String -> SDL.Font.Color -> V2 Double -> System' ()
spawnFloatingText s c (V2 x y) = void $ newEntity (FloatingText s c, Position (V2 (x + ht) y))
  where ht = let (V2 t _) = tileSize in fromIntegral t * 0.5

-- Make floating text float up
floatTooltips :: Double -> System' ()
floatTooltips dt = 
  cmap (\(FloatingText _ _, Position (V2 x y)) -> 
    if y > (-50) 
       then Just $ Position (V2 x (y - (dt * 0.1)))
       else Nothing
  )

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

-- Conversion from Int Rectangle to CInt Rectangle
toCIntRect :: Rectangle Int -> Rectangle CInt
toCIntRect (Rectangle (P (V2 x y)) (V2 i j)) = 
  Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral i) (fromIntegral j))

-- Conversion from Int Vector to CInt Vector
toCIntV2 :: V2 Double -> V2 CInt
toCIntV2 (V2 x y) = V2 (round x) (round y)

worldScale :: Double
worldScale = 32

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

tileSize :: V2 Int
tileSize = V2 32 32
tileSize' :: V2 CInt
tileSize' = V2 32 32

