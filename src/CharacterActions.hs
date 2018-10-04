{-# LANGUAGE ScopedTypeVariables #-}

module CharacterActions
( getReaction
, getNameColor
, attack
, spendEnergy
, shareTargetTo
) where

import Apecs
import SDL.Vect
import SDL.Font

import System.Random
import Control.Monad(when)
import Data.Matrix
import Data.HashMap.Strict as HM

import Common hiding (Left, Right, Down, Up)
import qualified Common as C
import Components
import Characters
import ActionStep

-- Determines how the target will react against the character
getReaction :: RelationshipTable -> Character -> Character -> Attitude
getReaction r char target =
  if faction char == faction target then Friendly
  else
    case HM.lookup (faction char) r of
      Just t -> 
        case HM.lookup (faction target) t of
          Just a -> a
          _ -> defaultReaction
      _ -> defaultReaction
    where defaultReaction
            | nature char == Aggressive = Hostile
            | nature char == Passive = Friendly
            | otherwise = Neutral

-- Get the name colour based on relationship with player
getNameColor :: Character -> System' SDL.Font.Color
getNameColor char = do
  Relationships r <- get global
  [(Player, pchar :: Character)] <- getAll
  pure $ case getReaction r char pchar of
    Friendly -> V4 100 255 100 255
    Hostile -> V4 255 50 50 255
    _ -> V4 150 150 150 255

-- Make one character attack another
-- The attacker incurs energy cost no matter what
attack :: Entity -> Entity -> System' ()
attack a v = do
  ac :: Character <- get a
  vc :: Character <- get v
  spendEnergy a 100
  Position pos <- get v
  damage <- liftIO $ getDamage ac vc
  let vc' = dealDamage a damage vc
      colour = getHealthColour (health vc') (maxHealth $ combatStats vc')
  set v vc'
  spawnFloatingText (show damage) colour pos
  aCol <- getNameColor ac
  vCol <- getNameColor vc
  postMessage $ if health vc' > 0 
  then [MBit (name ac, aCol), MBit " attacks ", MBit (name vc', vCol), MBit " for ", MBit (show damage, V4 255 50 50 255 :: SDL.Font.Color), MBit " damage!"]
  else [MBit (name ac, aCol), MBit " kills ", MBit (name vc', vCol) , MBit " with ", MBit (show (negate $ health vc'), V4 255 50 50 255 :: SDL.Font.Color), MBit " overkill damage!"]

-- Shares the target to the other character
shareTargetTo :: Entity -> Entity -> System' ()
shareTargetTo e f = do
  ec :: Character <- get e
  fc :: Character <- get f
  let enemy = target ec
  set f $ fc { target = enemy }
  spendEnergy e 100
  case enemy of
    Just ent -> do
      alertForPlayer <- exists ent (Proxy :: Proxy Player)
      when (alertForPlayer && target fc /= enemy) $ do
        p :: Character <- get ent
        ecCol <- getNameColor ec
        fcCol <- getNameColor fc
        pCol <- getNameColor p
        postMessage [MBit (name ec, ecCol), MBit " just alerted ", MBit (name fc, fcCol), MBit " of ", MBit (name p, pCol), MBit "'s presence!"]
    _ -> pure ()

-- Get the damage to be dealt using the IO monad
getDamage :: Character -> Character -> IO Int
getDamage atk def = do
  r <- getStdRandom (randomR (1, strength $ stats atk))
  let dam = r
      def = 0
  pure $ max (dam - def) 0

-- Deal simple damage to enemy health
dealDamage :: Entity -> Int -> Character -> Character
dealDamage attacker d c 
  | d > 0 = c { health = health c - d
              , regenTimer = max (regenTimer c) (d * 10) 
              , target = Just attacker}
  | otherwise = c

-- Make a character spend energy
spendEnergy :: Entity -> Int -> System' ()
spendEnergy e cost = do
  c :: Character <- get e
  set e $ c {energy = cost}
