module GameMap 
( getTile
, pathfind
, generateBlankMap
, generateIdentityMap
) where

import Apecs
import SDL hiding (Vector)
import Data.Graph.AStar

import Data.List
import Data.HashSet (HashSet, fromList)
import Data.Matrix hiding (getElem)
import Data.Vector(imap, ifoldl, toList)
import Debug.Trace (traceShow)

import Common
import Components

-- Matrix accessor with V2 support
getTile :: Matrix Tile -> V2 Int -> Maybe Tile
getElem :: Matrix a -> V2 Int -> Maybe a
getTile = getElem
getElem m (V2 x y) = safeGet (x+1) (y+1) m

-- Iterate through the matrix via folding
foldMatrix :: Matrix a -> (b -> V2 Int -> a -> b) -> b -> b
foldMatrix m func s = ifoldl (\p i n -> func p (pos i) n) s mat
  where pos i = let c = ncols m in V2 (i `mod` c) (i `div` c)
        mat = getMatrixAsVector m

-- Iterate through the matrix mapping 1 to 1
mapMatrix :: Matrix a -> (V2 Int -> a -> b) -> Matrix b
mapMatrix m func = Data.Matrix.fromList (nrows m) (ncols m) vec
  where vec = Data.Vector.toList $ imap (\i n -> func (pos i) n) mat
        pos i = let c = ncols m in V2 (i `mod` c) (i `div` c)
        mat = getMatrixAsVector m

-- Easy function for a blank map
generateBlankMap :: V2 Int -> Tile -> Matrix Tile
generateBlankMap (V2 w h) t = matrix w h (const t)

-- Identity map
generateIdentityMap :: V2 Int -> Matrix Tile
generateIdentityMap (V2 w h) = matrix w h (\(x, y) ->
  if x == y then Solid else Empty)

-- Pathfind from a point
pathfind :: Matrix Tile -> V2 Int ->  V2 Int -> Maybe [V2 Int]
pathfind m start dest = 
  aStar (findNeighbours m) nextDist dist ( == dest) start
    where dist s = calcDistance $ dest - s

-- Calculate distance between neighbours for astar
nextDist :: V2 Int -> V2 Int -> Double
nextDist current next = if x + y == 2 then 1.5 else fromIntegral $ x + y
  where (V2 x y) = abs <$> next - current

-- Calculate distance between two points
calcDistance :: V2 Int -> Double
calcDistance (V2 i j) = sqrt $ fromIntegral $ (i * i) + (j * j)

-- Finds a list of valid neighbours
findNeighbours :: Matrix Tile -> V2 Int -> HashSet (V2 Int)
findNeighbours m p = Data.HashSet.fromList $ filter findT searches
  where searches = [p + V2 i j | i <- [-1..1], j <- [-1..1], not (i == 0 && j == 0)]
        findT pos = checkT $ getTile m pos 
        checkT (Just t) = t == Empty
        checkT _ = False
