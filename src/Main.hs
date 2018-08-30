{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# OPTIONS_GHC -Wall #-}

import Apecs 

import SDL.Vect
import SDL(($=))
import qualified SDL
import qualified SDL.Image(quit)

import Control.Monad
import System.Exit (exitSuccess)

import Common
import Components
import EventHandler
import ImageLoad
import GameMap

-- Initialises the world with it's first system:
-- this system simply creates an entity
initialise :: Resources -> System' ()
initialise r = void $ do
  modify global (\(Textures _) -> Textures $ createTextureComp r)
  modify global (\(GameMap _) -> GameMap $ generateBlankMap (V2 50 50) Empty)
  newEntity 
    ( Player
    , Position playerPos
    , CellRef playerCellRef
    , Sprite "Assets/sprites.png" (SDL.Rectangle (P (V2 32 32)) (V2 16 16)))

-- When called, manipulates the global time component
incrTime :: Double -> System' ()
incrTime dT = modify 0 $ \(Time t) -> Time (t+dT)

-- Converts cell references to game position
snapEntities :: System' ()
snapEntities = 
  cmap $ \(Position (V2 _ _), CellRef (V2 x y)) ->
    Position (V2 (calc x) (calc y))
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
  snapEntities

-- Produce a system used for drawing
drawComponents :: Get World c => (c -> Position -> IO ()) -> System' (IO ())
drawComponents f = cfold (\img (p, comp) -> img <> f comp p) mempty

-- Create System' (IO ()) for everything depending on item drawn
draw :: SDL.Renderer -> System' (IO ())
draw renderer = do
  Textures texs <- get global
  sequence_ <$> sequence 
    [ drawComponents $ renderSprite renderer texs
    , printMessages
    ]

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
        runSystem clearMessages world

        SDL.present renderer
        unless quit $ loop ticks

  -- Begin looping
  loop 0
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.quit
  exitSuccess


