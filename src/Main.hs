{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}

import Apecs
import Apecs.Core
import Apecs.Stores
import Apecs.Util

import Linear
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random
import Control.Monad
import Data.Monoid

newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

newtype Velocity = Velocity (V2 Double) deriving Show
instance Component Velocity where type Storage Velocity = Map Velocity

data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

makeWorld "World" [''Time, ''Player, ''Position, ''Velocity] 

type System' a = System World a
type Kinetic = (Position, Velocity)

playerSpeed, xmin, xmax :: Double
playerSpeed = 170
xmin = -500
xmax = 500

playerPos:: V2 Double
playerPos = V2 0 (-120)

initialise :: System' ()
initialise = void $ newEntity (Player, Position playerPos, Velocity 0)

stepPosition :: Double -> System' ()
stepPosition dT = cmap $ \(Position p, Velocity v) -> Position (p + dT *^ v)

clampPlayer :: System' ()
clampPlayer = cmap $ \(Player, Position (V2 x y))
                   -> Position (V2 (min xmax . max xmin $ x) y)

incrTime :: Double -> System' ()
incrTime dT = modify 0 $ \(Time t) -> Time (t+dT)

triggerEvery :: Double -> Double -> Double -> System' a -> System' ()
triggerEvery dT period phase sys = do
  Time t <- get global
  let t' = t + phase
      trigger = floor (t'/period) /= floor ((t'+dT)/period)
  when trigger $ void sys

step :: Double -> System' ()
step dT = do
  incrTime dT
  stepPosition dT
  clampPlayer

handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x-playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyLeft)  Up   _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x+playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x+playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyRight) Up   _ _) =
  cmap $ \(Player, Velocity (V2 x y)) -> Velocity (V2 (x-playerSpeed) y)

handleEvent (EventKey (SpecialKey KeySpace) Down _ _) =
  cmap $ \(Player, Velocity (V2 x _)) -> Velocity (V2 x playerSpeed)

handleEvent _ = return ()

drawComponents :: Get World c => (c -> Picture) -> System' Picture
drawComponents f = cfold
  (\pic (Position p, c) -> pic <> translate' p (f c))
  mempty

translate' :: V2 Double -> Picture -> Picture
translate' (V2 x y) = translate (realToFrac x) (realToFrac y)

square :: Picture
square = Line [(0.5,0.5),(0.5,-0.5),(-0.5,-0.5),(-0.5,0.5),(0.5,0.5)]

draw :: System' Picture
draw = drawComponents $ \Player -> color white . scale 10 10 $ square

playGloss :: w
          -> System w Picture
          -> (Event -> System w ())
          -> (Double -> System w ())
          -> IO ()
playGloss world drawSys eventSys stepSys =
  playIO
    window black fps ()
    (\_    -> runSystem drawSys world)
    (\e _  -> runSystem (eventSys e) world)
    (\dt _ -> runSystem (stepSys $ realToFrac dt) world)
  where
    window = InWindow "App" (2560,1400) (10,10)
    fps = 200

main :: IO ()
main = do
  w <- initWorld
  runSystem initialise w
  playGloss w draw handleEvent step
