{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs 
import Apecs.Core
import Apecs.Stores
import Apecs.Util
import Apecs.System

import SDL.Vect
import SDL(($=))
import qualified SDL
import qualified SDL.Image(quit)

import Control.Monad
import Data.Monoid
import Data.Maybe

import System.Exit (exitSuccess)
import qualified Data.HashMap as HM

import Common
import Components
import EventHandler
import ImageLoad
import GameMap

-- Initialises the world with it's first system:
-- this system simply creates an entity
initialise :: Resources -> System' ()
initialise r = void $ do
  modify global (\(TextureComp _) -> TextureComp $ createTextureComp r)
  modify global (\(GameMapComp _) -> GameMapComp $ generateBlankMap (V2 50 50) Empty)
  newEntity 
    ( Player
    , Position playerPos
    , CellRef playerCellRef
    , Sprite "Assets/sprites.png" (SDL.Rectangle (P (V2 32 32)) (V2 16 16)))

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

-- Produce a system used for drawing
drawComponents :: Get World c => (Textures -> c -> Position -> IO ()) -> System' (IO ())
drawComponents f = do
  TextureComp texs <- get global
  cfold (\img (p, comp) -> img <> f texs comp p) mempty

-- Create System' (IO ()) for everything depending on item drawn
draw :: SDL.Renderer -> System' (IO ())
draw renderer = drawComponents $ renderSprite renderer

-- Main program thread
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

        join $ runSystem (draw renderer) world

        SDL.present renderer
        unless quit $ loop ticks

  -- Begin looping
  loop 0
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.quit
  exitSuccess
