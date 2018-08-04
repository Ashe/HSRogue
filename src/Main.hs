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
import Data.Maybe

import Common
import Components
import EventHandler
import ImageLoad

initialise :: Maybe Picture -> System' ()
initialise sp = void $ newEntity (Player, Position playerPos, Sprite sp)

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

drawComponents :: Get World comp => (comp -> Picture) -> System' Picture
drawComponents picFunc = cfold
  (\pic (Position p, comp) -> pic <> translate' p (picFunc comp))
  mempty

translate' :: V2 Int -> Picture -> Picture
translate' (V2 x y) = translate (realToFrac x) (realToFrac y)

square :: Picture
square = Line [(0.5,0.5),(0.5,-0.5),(-0.5,-0.5),(-0.5,0.5),(0.5,0.5)]

draw :: System' Picture
draw = drawComponents $ \(Sprite maybePic) -> fromMaybe Blank maybePic 

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
  img <- getSprite "Assets/sprites.png"
  runSystem (initialise img) w
  playGloss w draw handleEvent step
