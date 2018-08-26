module EventHandler
( handlePayload
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

-- Handle the entire event payload
handlePayload :: [EventPayload] -> System' ()
handlePayload = foldl (\_ ev -> handleEvent ev) (pure ()) 
  
-- The main event handler function for dealing with keypresses
handleEvent :: EventPayload -> System' ()
handleEvent ev = 
  case ev of
    KeyboardEvent ev -> handleKeyEvent ev
    _ -> pure ()

handleKeyEvent :: KeyboardEventData -> System' ()
handleKeyEvent ev = 
  case keyboardEventKeyMotion ev of
    Pressed -> movePlayer $ findDir $ keysymScancode $ keyboardEventKeysym ev
    _ -> pure ()

-- Move the player in a direction using move speed
movePlayer :: Maybe Direction -> System' ()
movePlayer (Just dir) = 
  let (V2 i j) = directionToVect dir in
  cmap $ \(Player, CellRef (V2 x y)) -> CellRef (V2 (x + i * playerSpeed) (y + j * playerSpeed))
movePlayer _ = pure ()

-- Find a direction of movement from scancode
findDir :: Scancode -> Maybe Direction
findDir ScancodeW = Just C.Up
findDir ScancodeUp = Just C.Up
findDir ScancodeD = Just C.Right
findDir ScancodeRight = Just C.Right
findDir ScancodeS = Just C.Down
findDir ScancodeDown = Just C.Down
findDir ScancodeA = Just C.Left
findDir ScancodeLeft = Just C.Left
findDir _ = Nothing
