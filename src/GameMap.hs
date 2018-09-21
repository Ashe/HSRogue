module GameMap 
( getTile
, generateBlankMap
, generateIdentityMap
) where

import SDL hiding (Vector)
import Data.Matrix
import Data.Vector(ifoldl)
import Debug.Trace (traceShow)

import Common
import Components

-- Matrix accessor with V2 support
getTile :: Matrix Tile -> V2 Int -> Maybe Tile
getTile m p@(V2 x y) = safeGet (x+1) (y+1) m

-- Iterate through the matrix easily
iterateMatrix :: Matrix Tile -> (a -> V2 Int -> Tile -> a) -> a -> a
iterateMatrix m func s = ifoldl (\p i n -> func p (pos i) n) s mat
  where pos i = let c = ncols m in V2 (i `mod` c) (i `div` c)
        mat = getMatrixAsVector m

-- Easy function for a blank map
generateBlankMap :: V2 Int -> Tile -> Matrix Tile
generateBlankMap (V2 w h) t = matrix w h (const t)

-- Identity map
generateIdentityMap :: V2 Int -> Matrix Tile
generateIdentityMap (V2 w h) = matrix w h (\(x, y) ->
  if x == y then Solid else Empty)
