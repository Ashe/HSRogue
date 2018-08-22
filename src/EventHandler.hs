module EventHandler
( handleEvent
) where

import Apecs
import Apecs.Core
import Apecs.Stores
import Apecs.Util
import Apecs.System

import SDL

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
  
-- The main event handler function for dealing with keypresses
handleEvent :: Event -> System' ()
handleEvent (Event _ payload) = 
  case payload of
    KeyboardEvent ev -> handleKeyEvent ev
    _ -> pure ()

handleKeyEvent :: KeyboardEventData -> System' ()
handleKeyEvent ev = 
  case keyboardEventKeyMotion ev of
    Pressed -> movePlayer $ findDir $ keysymScancode $ keyboardEventKeysym ev
    _ -> pure ()

-- Move the player in a direction using move speed
movePlayer :: Direction -> System' ()
movePlayer dir = 
  let (V2 i j) = directionToVect dir in
  cmap $ \(Player, CellRef (V2 x y)) -> CellRef (V2 (x + i * playerSpeed) (y + j * playerSpeed))

-- Find a direction of movement from scancode
findDir :: Scancode -> Direction
findDir ScancodeW = C.Up
findDir ScancodeUp = C.Up
findDir ScancodeD = C.Right
findDir ScancodeRight = C.Right
findDir ScancodeS = C.Down
findDir ScancodeDown = C.Down
findDir ScancodeA = C.Left
findDir ScancodeLeft = C.Left
