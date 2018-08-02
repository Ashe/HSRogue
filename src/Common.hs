{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common
( World(..)
, initWorld
, System'
, playerPos
, playerSpeed
) where

import Apecs
import Linear

import Components

-- Uses templateHaskell to create the data 'World'
makeWorld "World" [''Time, ''Player, ''Position] 

-- Easy type synonym for systems
type System' a = System World a

playerPos :: V2 Int
playerPos = V2 0 0

playerSpeed :: Int
playerSpeed = 1

