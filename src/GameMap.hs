module GameMap 
( GameMap(..)
, Row(..)
, Tile(..)
, getTile
, generateBlankMap
) where

import SDL.Vect hiding (Vector)
import Data.Vector

-- Data for describing the traversability of a tile
data Tile = Empty | Solid

-- Easy type synonyms
type Row = Vector Tile
type GameMap = Vector Row

-- Easy function for accessing the game map
getTile :: GameMap -> V2 Int -> Maybe Tile
getTile gm (V2 x y) = genMap (gm !? y)
  where genMap (Just row) = row !? x
        genMap _ = Nothing

-- Trivial map filled with the given tile
generateBlankMap :: V2 Int -> Tile -> GameMap
generateBlankMap (V2 x y) t = generate y (const $ generate x (const t))
