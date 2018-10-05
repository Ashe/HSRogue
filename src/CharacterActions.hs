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
import Data.HashMap.Strict as HM

import Common
import Components
import Characters

-- Determines how the target will react against the character
getReaction :: RelationshipTable -> CharacterInfo -> CharacterInfo -> Attitude
getReaction r char tar =
  if faction char == faction tar then Friendly
  else
    case HM.lookup (faction char) r of
      Just t -> 
        case HM.lookup (faction tar) t of
          Just a -> a
          _ -> defaultReaction
      _ -> defaultReaction
    where defaultReaction
            | nature char == Aggressive = Hostile
            | nature char == Passive = Friendly
            | otherwise = Neutral

-- Get the name colour based on relationship with player
getNameColor :: CharacterInfo -> System' SDL.Font.Color
getNameColor char = do
  Relationships r <- get global
  [(Player, Character pchar)] <- getAll
  pure $ case getReaction r char pchar of
    Friendly -> V4 100 255 100 255
    Hostile -> V4 255 50 50 255
    _ -> V4 150 150 150 255

-- Make one character attack another
-- The attacker incurs energy cost no matter what
attack :: Entity -> Entity -> System' ()
attack a v = do
  Character ac <- get a
  Character vc <- get v
  spendEnergy a 100
  Position pos <- get v
  damage <- liftIO $ getDamage ac vc
  let vc' = dealDamage a damage vc
      colour = getHealthColour (health vc') (maxHealth $ combatStats vc')
  set v $ Character vc'
  spawnFloatingText (show damage) colour pos
  aCol <- getNameColor ac
  vCol <- getNameColor vc
  postMessage $ if health vc' > 0 
  then [MBit (name ac, aCol), MBit " attacks ", MBit (name vc', vCol), MBit " for ", MBit (show damage, V4 255 50 50 255 :: SDL.Font.Color), MBit " damage!"]
  else [MBit (name ac, aCol), MBit " kills ", MBit (name vc', vCol) , MBit " with ", MBit (show (negate $ health vc'), V4 255 50 50 255 :: SDL.Font.Color), MBit " overkill damage!"]

-- Shares the target to the other character
shareTargetTo :: Entity -> Entity -> System' ()
shareTargetTo e f = do
  Character ec <- get e
  Character fc <- get f
  let enemy = target ec
  set f $ Character $ fc { target = enemy }
  spendEnergy e 100
  case enemy of
    Just ent -> do
      alertForPlayer <- exists ent (Proxy :: Proxy Player)
      when (alertForPlayer && target fc /= enemy) $ do
        Character p <- get ent
        ecCol <- getNameColor ec
        fcCol <- getNameColor fc
        pCol <- getNameColor p
        postMessage [MBit (name ec, ecCol), MBit " just alerted ", MBit (name fc, fcCol), MBit " of ", MBit (name p, pCol), MBit "'s presence!"]
    _ -> pure ()

-- Get the damage to be dealt using the IO monad
getDamage :: CharacterInfo -> CharacterInfo -> IO Int
getDamage atk def = do
  r <- getStdRandom (randomR (1, strength $ stats atk))
  let dam = r
      def = 0
  pure $ max (dam - def) 0

-- Deal simple damage to enemy health
dealDamage :: Entity -> Int -> CharacterInfo -> CharacterInfo
dealDamage attacker d c 
  | d > 0 = c { health = health c - d
              , regenTimer = max (regenTimer c) (d * 10) 
              , target = Just attacker}
  | otherwise = c

-- Make a character spend energy
spendEnergy :: Entity -> Int -> System' ()
spendEnergy e cost = do
  Character c <- get e
  set e $ Character $ c {energy = cost}
