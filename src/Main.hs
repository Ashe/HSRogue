{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs 
import Apecs.Core
import Apecs.Stores
import Apecs.Util

import SDL.Vect
import SDL(($=))
import qualified SDL

import Control.Monad
import Data.Monoid
import Data.Maybe

import System.Exit (exitSuccess)

import Common
import Components
import EventHandler
import ImageLoad

-- Initialises the world with it's first system:
-- this system simply creates an entity
initialise :: System' ()
initialise = void $ newEntity 
  ( Player
  , Position playerPos
  , CellRef playerCellRef)

-- When called, manipulates the global time component
incrTime :: Double -> System' ()
incrTime dT = modify 0 $ \(Time t) -> Time (t+dT)

-- Converts cell references to game position
snapEntities :: Double -> System' ()
snapEntities dT =
  cmap $ \(Position (V2 x y), CellRef (V2 cellX cellY)) ->
    Position (V2 (calc cellX) (calc cellY))
      where calc n = worldScale * fromIntegral n

triggerEvery :: Double -> Double -> Double -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

step :: Double -> System' ()
step dT = do
  incrTime dT
  snapEntities dT

--drawComponents :: Get World comp => (comp -> Picture) -> System' Picture
--drawComponents picFunc = cfold
--  (\pic (Position p, comp) -> pic <> translate' p (picFunc comp))
--  mempty

-- translate' :: V2 Double -> Picture -> Picture
-- translate' (V2 x y) = translate (realToFrac x) (realToFrac y)

-- draw :: System' Picture
-- draw = drawComponents $ \(Sprite maybePic) -> fromMaybe Blank maybePic 

main :: IO ()
main = do
  w <- initWorld
  -- img <- getSprite "Assets/sprites.png"
  
  -- Initialise SDL with a window
  SDL.initialize [SDL.InitVideo]
  window <- SDL.createWindow "App" SDL.defaultWindow
  renderer <-
      SDL.createRenderer window (-1)
        SDL.RendererConfig
          { SDL.rendererType = SDL.AcceleratedRenderer
          , SDL.rendererTargetTexture = False
          }

  SDL.showWindow window

  let loop = do
        events <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` events

        SDL.rendererDrawColor renderer $= V4 0 0 0 0
        SDL.clear renderer
        SDL.present renderer
        unless quit loop

  -- Begin looping
  loop 
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  exitSuccess
