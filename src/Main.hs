{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
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

import Common
import Components
import EventHandler

initialise :: System' ()
initialise = void $ newEntity (Player, Position playerPos)

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

drawComponents :: Get World c => (c -> Picture) -> System' Picture
drawComponents f = cfold
  (\pic (Position p, c) -> pic <> translate' p (f c))
  mempty

translate' :: V2 Int -> Picture -> Picture
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
