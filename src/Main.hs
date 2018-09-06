{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}

import Apecs 

import SDL.Vect
import SDL(($=))
import qualified SDL
import qualified SDL.Image(quit)
import qualified SDL.Font
import Data.Fixed

import Control.Monad
import System.Exit (exitSuccess)

import Common
import Components
import EventHandler
import Resources
import GameMap
import Characters

-- Initialises the world with it's first system:
-- this system simply creates an entity
initialise :: [TexResource] -> [FontResource] -> System' ()
initialise t f = void $ do
  modify global (\(Textures _) -> Textures $ createResourceMap t)
  modify global (\(Fonts _) -> Fonts $ createResourceMap f)
  modify global (\(GameMap _) -> GameMap $ generateBlankMap (V2 20 20) Empty)
  newEntity 
    ( Player
    , Position playerPos
    , CellRef playerCellRef
    , Character "You" 100 100 Neutral
    , Sprite "Assets/sprites.png" (SDL.Rectangle (P (V2 16 16)) (V2 16 16)))
  newEntity 
    ( Position (V2 0 0)
    , CellRef (V2 8 10)
    , Character "Chum" 100 100 Friendly
    , Sprite "Assets/sprites.png" (SDL.Rectangle (P (V2 112 64)) (V2 16 16)))
  newEntity 
    ( Position (V2 0 0)
    , CellRef (V2 12 10)
    , Character "Tum" 100 100 Aggressive
    , Sprite "Assets/sprites.png" (SDL.Rectangle (P (V2 112 16)) (V2 16 16)))
  actionStep

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
draw :: SDL.Renderer -> Int -> System' (IO ())
draw renderer fps = do
  Textures texs <- get global
  Fonts fonts <- get global
  sequence_ <$> sequence 
    [ renderWorld renderer
    , drawComponents $ renderSprite renderer texs
    , drawComponents $ renderReticule renderer
    , printMessages
    , displayFps renderer fps fonts "Assets/Roboto-Regular.ttf"
    ]

-- Main program thread
main :: IO ()
main = do
  world <- initWorld
  
  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  -- Create a window and renderer
  window <- SDL.createWindow "App" SDL.defaultWindow
  renderer <-
      SDL.createRenderer window (-1)
        SDL.RendererConfig
          { SDL.rendererType = SDL.AcceleratedRenderer
          , SDL.rendererTargetTexture = False
          }

  -- Load resources and initialise game
  texs <- loadTextures renderer ["Assets/sprites.png"]
  fonts <- loadFonts [("Assets/Roboto-Regular.ttf", 12)]
  runSystem (initialise texs fonts) world

  -- Display the game
  SDL.showWindow window

  let loop prevTicks secondTick fpsAcc prevFps = do
        ticks <- SDL.ticks
        payload <- map SDL.eventPayload <$> SDL.pollEvents
        let quit = SDL.QuitEvent `elem` payload
            dt = ticks - prevTicks
            calcFps = secondTick + dt > 1000
            newFps = if calcFps then fpsAcc + 1 else prevFps 
            newFpsAcc = if calcFps then 1 else fpsAcc + 1
            newSecondTick = if calcFps then mod (secondTick + dt) 1000 else secondTick + dt

        runSystem (handlePayload payload) world
        runSystem (step $ fromIntegral dt) world

        SDL.rendererDrawColor renderer $= V4 0 0 0 0
        SDL.clear renderer

        join $ runSystem (draw renderer newFps) world
        runSystem clearMessages world

        SDL.present renderer
        unless quit $ loop ticks newSecondTick newFpsAcc newFps

  -- Begin looping
  loop 0 0 0 0
  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.Font.quit
  SDL.quit
  putStrLn "Goodbye! (◠‿◠✿)"
  exitSuccess


