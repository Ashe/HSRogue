module GameMap 
( Grid(..)
, Row(..)
, Tile(..)
, getTile
, generateBlankMap
) where

import SDL hiding (Vector)
import Data.Vector
import Foreign.C

-- Data for describing the traversability of a world tile
data Tile = Empty | Solid

-- Easy type synonyms
type Row = Vector Tile
type Grid = Vector Row

-- Easy function for accessing the game map
getTile :: Grid -> V2 Int -> Maybe Tile
getTile gm (V2 x y) = genMap (gm !? y)
  where genMap (Just row) = row !? x
        genMap _ = Nothing

-- Trivial map filled with the given tile
generateBlankMap :: V2 Int -> Tile -> Grid
generateBlankMap (V2 x y) t = generate y (const $ generate x (const t))

