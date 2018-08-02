{-# LANGUAGE TypeFamilies #-}

module Components
( Player(..)
, Time(..)
, Position(..)
) where

import Apecs
import Apecs.Stores
import Linear

-- Global component, exists outside of entities
-- Used for managing the passage of time
newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

-- Unique component, either one or none exists
data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

-- Used to store the position of game entities
newtype Position = Position (V2 Int) deriving Show
instance Component Position where type Storage Position = Map Position

