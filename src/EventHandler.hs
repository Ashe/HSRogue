module EventHandler
( handleEvent
) where

import Apecs
import Apecs.Core
import Apecs.Stores
import Apecs.Util
import Apecs.System

import Linear
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Common
import Components
  
-- The main event handler function for dealing with keypresses
handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 (x-playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 (x+playerSpeed) y)

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 x (y+playerSpeed))

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 x (y-playerSpeed))

handleEvent _ = return ()
