{-# LANGUAGE ScopedTypeVariables #-}

module WorldSimulation
( simulateWorld
, readyPlayer
, navigatePlayer
, executePlayerPath
, playerActionStep
) where

import Apecs
import SDL.Vect

import System.Random
import Data.Maybe (isJust)
import Data.Matrix
import Data.List(find)
import Control.Monad(when, unless)

import Types hiding (Left, Right, Down, Up)
import Common
import Components
import Characters
import CharacterActions
import ActionStep
import GameMap

-- Simulate the world, advancing time until the player must act
simulateWorld :: System' ()
simulateWorld = do
  set global $ PlayerReady False
  snapEntities
  simulateCharacters
  regenHealthAndEnergy
  [(Player, Character c)] <- getAll
  if energy c > 0
    then simulateWorld
    else readyPlayer

-- Get the player ready for their turn
-- Run systems that only affect the player
readyPlayer :: System' ()
readyPlayer = do
  set global $ PlayerReady True
  writeExamines

-- Make the player walk to location
executePlayerPath :: V2 Int -> System' ()
executePlayerPath pos = do
  PlayerPath path <- get global
  case path of
    [] -> pure ()
    (x : xs) ->
      case vectToDirection (x - pos) of
        Just dir -> do
          set global $ PlayerReady False
          set global $ PlayerPath xs
          navigatePlayer dir
        Nothing ->
          cancelPlayerPath

-- Cancel the player's pathing
cancelPlayerPath :: System' ()
cancelPlayerPath = do
  set global $ PlayerPath []
  set global $ PlayerReady True

-- The player has made their move and is ready to simulate
-- This spends the player's energy
playerActionStep :: Int -> System' ()
playerActionStep cost = do
  when (cost > 0) $ do
    [(Player,Character c, p)] <- getAll
    set p $ Character $ c { energy = cost}
  actionStep
  simulateWorld

-- Easy type synonym
type Comps = (Character, CellRef, Entity)

-- Make all non-player characters act if they have enough energy
simulateCharacters :: System' ()
simulateCharacters = do
  GameMap m <- get global
  ls :: [Comps] <- getAll
  mapM_ (manipulateCharacter m ls) ls

-- Regenerate every character's health and decrease cooldowns
regenHealthAndEnergy :: System' ()
regenHealthAndEnergy = cmapM (\(Character c, Position p) -> do
  let allowRegen = regenTimer c <= 0
      hr = healthRegen $ combatStats c
      hrCapped = min (max 0 (maxHealth (combatStats c) - health c)) hr
  when (allowRegen && hrCapped > 0) $ spawnFloatingText (show hrCapped) (V4 0 255 0 255) p
  pure $ Character $ c 
    { health = 
      if allowRegen && hrCapped > 0
        then health c + hrCapped
        else health c
    , regenTimer = 
      if allowRegen
        then 10
        else regenTimer c - 1
    , energy = max 0 $ energy c - energyRegen (combatStats c)})

-- Place important information into examine messages
writeExamines :: System' ()
writeExamines = 
  cmapM (\(Character c@(CharacterInfo cname h e _ cstats cbStats f cnature t)) -> do
    charCol <- getNameColor c
    let healthCol = getHealthColour h (maxHealth cbStats)
    pure $ Examine 
      [MBit (cname, charCol), MBit ": ", MBit (f, charCol), MBit $ ", " ++ show cnature ++ ", ", MBit ("Health: " ++ show h ++ "/" ++ show (maxHealth cbStats), healthCol)])

-- Manipulate each character with respect to the map and other chars
-- This is a BIG function. For now, check attitude and attack the player
manipulateCharacter :: Matrix Tile -> [Comps] -> Comps ->  System' ()
manipulateCharacter gm ls comps@(Character c, CellRef p, e) =
  unless (energy c > 0) $ 
    case nature c of
      Passive ->
        idleCharacter e c
      Aggressive -> do
        tar <- acquireTargets ls comps
        set e $ Character $ c { target = tar }
        pursueTarget e c tar
      Defensive ->
        pursueTarget e c $ target c

-- Returns the current target or a new target
acquireTargets :: [Comps] -> Comps -> System' (Maybe Entity)
acquireTargets ls (Character char, CellRef pos, e) =
  if isJust $ target char then pure $ target char
  else do
    Relationships rships <- get global
    let r = visionRange $ combatStats char
        search = [ CellRef $ pos + V2 i j | i <- [-r .. r], j <- [-r .. r]
                 , (i*i) + (j*j) <= r*r && not (i == 0 && j == 0)]
        es = filter (\(Character c, p, _) -> p `elem` search && getReaction rships char c == Hostile ) ls
    if null es then pure Nothing
    else do
      choice <- liftIO $ getStdRandom (randomR (0, length es - 1))
      let (_, _, tar) = es !! choice
      pure $ Just tar

-- Follow and attack the current target, if there is one
pursueTarget :: Entity -> CharacterInfo -> Maybe Entity -> System' ()
pursueTarget e c tar =
  let idle = idleCharacter e c in
  case tar of
    Just t -> do
      GameMap m <- get global
      CellRef pos <- get e
      CellRef end <- get t
      let path = pathfind m pos end
      case path of
        Just paths ->
          let dir = vectToDirection $ head paths - pos in
          case dir of
            Just d ->
              navigateNPC e pos c d
            _ -> idle
        _ -> idle
    _ -> idle

-- Make an npc move in a direction
navigateNPC :: Entity -> V2 Int -> CharacterInfo -> Direction -> System' ()
navigateNPC e pos c dir = do
  ls :: CharacterList <- getAll
  let dest = pos + directionToVect dir
      entOnSpace = find (\(_, CellRef p, _) -> p == dest) ls
  case entOnSpace of
    Just (Character tar, _, ent) -> do
      Relationships rships <- get global
      case getReaction rships c tar of
        Hostile -> e `attack` ent
        Friendly -> e `shareTargetTo` ent
        _ -> 
          case nature c of
            Aggressive -> e `attack` ent
            _ -> spendEnergy e 100
    _ -> do
      set e $ CellRef dest
      spendEnergy e 100
  actionStep

-- Make a character wait
idleCharacter :: Entity -> CharacterInfo -> System' ()
idleCharacter e c = spendEnergy e 100

-- Move, swap, or fight in a given direction, standard navigation
navigatePlayer :: Direction -> System' ()
navigatePlayer dir = do
  GameMap m <- get global
  [(Player, CellRef pos, Character pChar, p)] <- getAll
  chars :: CharacterList <- getAll
  let dest = pos + directionToVect dir
  action <- getNavAction m pChar dest chars
  case action of
    Left na ->
      case na of
        Move -> do
          set p $ CellRef dest
          playerActionStep 100
        Swap e c -> do
          set e $ CellRef pos
          set p $ CellRef dest
          cCol <- getNameColor c
          postMessage [MBit "You switch places with ", MBit (name c, cCol), MBit "!"]
          playerActionStep 100
        Fight e -> do
          p `attack` e
          playerActionStep 0
    Right msg -> do
      cancelPlayerPath
      postMessage msg

-- Things that can come from navigation
data NavAction = Move | Swap Entity CharacterInfo | Fight Entity

-- Given a direction, find how to execute the player's intent
getNavAction :: Matrix Tile -> CharacterInfo -> V2 Int -> CharacterList -> System' (Either NavAction [MBit])
getNavAction g p dest cs =
  case tile of
    Nothing -> pure $ Right [MBit "Hmm.. You can't find a way to move there."]
    Just t -> 
      if t == Empty
        then case charOnSpace of
          Nothing -> pure $ Left Move
          Just (Character c, _, e) -> do
            cCol <- getNameColor c
            Relationships r <- get global
            pure $ case getReaction r c p of
              Hostile -> Left $ Fight e
              Friendly -> Left $ Swap e c
              Neutral -> Right [MBit "Oof! You bumped into ",  MBit (name c, cCol), MBit "!"]
        else pure $ Right [MBit "Ouch! You bumped into a wall!"]
  where tile = getTile g dest
        chk (Character _, CellRef checkp, _) = dest == checkp
        charOnSpace = find chk cs

