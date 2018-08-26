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
import qualified Data.HashMap as HM

import Common
import Components
import EventHandler
import ImageLoad

import Debug.Trace as D

-- Initialises the world with it's first system:
-- this system simply creates an entity
initialise :: Resources -> System' ()
initialise r = void $ do
  modify 0 (\(Textures texs) -> createTextureComp r)
  newEntity 
    ( Player
    , Position playerPos
    , CellRef playerCellRef
    , Sprite "Assets/sprites.png" (SDL.Rectangle (P (V2 0 0)) (V2 50 50)))

-- When called, manipulates the global time component
incrTime :: Double -> System' ()
incrTime dT = modify 0 $ \(Time t) -> Time (t+dT)

-- Converts cell references to game position
snapEntities :: Double -> System' ()
snapEntities dT =
  cmap $ \(Position (V2 x y), CellRef (V2 cellX cellY)) ->
    Position (V2 (calc cellX) (calc cellY))
      where calc n = worldScale * fromIntegral n

-- Runs a system periodically
triggerEvery :: Double -> Double -> Double -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

-- Main step system
step :: Double -> System' ()
step dT = do
  incrTime dT
  snapEntities dT

-- Render things depending on their components
drawComponents :: Get World comp => SDL.Renderer -> (comp -> (String, SDL.Rectangle Int)) -> System' (IO ())
drawComponents r f = cfold
  (\img (Position p, comp, Textures texs) -> 
    let (fp, rect) = f comp in
      case HM.lookup fp texs of
      (Just tex) -> D.trace "RENDERING TEXTURE" img <> SDL.copyEx r tex (Just $ toCIntRect rect) Nothing 0 Nothing (V2 False False)
      _ -> D.trace "NO TEXTURE" img)
  mempty


-- Main draw system
draw :: SDL.Renderer -> System' (IO ())
draw renderer = D.trace "DRAW FUNC" drawComponents renderer $ \(Sprite fp rect) -> (fp, rect)

main :: IO ()
main = do
  world <- initWorld
  
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

  resources <- loadTextures renderer ["Assets/sprites.png"]
  runSystem (initialise resources) world

  let loop prevTicks = do
        ticks <- SDL.ticks
        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            dt = fromIntegral $ ticks - prevTicks

        runSystem (handlePayload payload) world
        runSystem (step dt) world

        SDL.rendererDrawColor renderer $= V4 0 0 0 0
        SDL.clear renderer

        runSystem (draw renderer) world

        SDL.present renderer
        unless quit $ loop ticks

  -- Begin looping
  loop 0
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
  exitSuccess
