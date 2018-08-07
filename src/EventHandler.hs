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

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
  
-- The main event handler function for dealing with keypresses
handleEvent :: Event -> System' ()
handleEvent (EventKey (SpecialKey KeyLeft) Down _ _) =
  movePlayer C.Left

handleEvent (EventKey (SpecialKey KeyRight) Down _ _) =
  movePlayer C.Right

handleEvent (EventKey (SpecialKey KeyUp) Down _ _) =
  movePlayer C.Up

handleEvent (EventKey (SpecialKey KeyDown) Down _ _) =
  movePlayer C.Down

handleEvent _ = return ()

-- Move the player in a direction using move speed
movePlayer :: Direction -> System' ()
movePlayer dir = 
  let (V2 i j) = directionToVect dir in
  cmap $ \(Player, Position (V2 x y)) -> Position (V2 (x + i * playerSpeed) (y + j * playerSpeed))
