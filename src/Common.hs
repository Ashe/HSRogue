{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
( World(..)
, initWorld
, System'
, FPS
, Direction(..)
, CharacterList
, postMessage
, printMessages
, clearMessages
, snapEntities
, spawnFloatingText
, floatTooltips
, getDMGPopupColour
, examinePos
, genSolidText
, genBlendedText

, directionToVect
, vectToDirection
, playerPos
, playerCellRef
, tileSize
, standardRange
) where

import Apecs
import SDL hiding (get, Vector, Renderer)
import qualified SDL
import SDL.Font
import Foreign.C
import Data.HashMap.Strict as HM
import Data.Text(Text, pack)
import Control.Monad(void)
import Control.Arrow((***))

import Data.Vector (ifoldl)

import Components
import Resources
import Characters

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''WindowSize, ''Messages, ''GameState, ''Textures, ''Renderer,''Fonts
                  , ''GameMap , ''Player, ''PlayerReady, ''PlayerPath, ''Relationships, ''Reticule
                  , ''Position , ''CellRef, ''Sprite , ''Character , ''Examine, ''FloatingTex] 

-- Easy type synonym for systems
type System' a = System World a

-- Easy FPS 
type FPS = Int

-- Types for Directions
data Direction = 
  Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving (Read, Show, Eq, Ord)

-- Easy way of getting all non-player entities
type CharacterList = [(Character, CellRef, Entity)]

-- Post a new message
postMessage :: String -> System' ()
postMessage [] = pure ()
postMessage m = modify global (\(Messages msgs) -> Messages $ m : msgs)

-- Print messages into console
printMessages :: System' (IO ())
printMessages = do
  Messages msgs <- get global
  pure $ foldl (\io m ->io <> putStrLn m) mempty $ reverse msgs

-- Flush any messages
clearMessages :: System' ()
clearMessages = modify global (\(Messages _) -> Messages [])

-- Converts cell references to game position
snapEntities :: System' ()
snapEntities = cmap (\(Position (V2 _ _), CellRef p) ->
  Position $ fromIntegral <$> p * tileSize)

-- Spawn a floating tooltip
spawnFloatingText :: String -> SDL.Font.Color -> V2 Double -> System' ()
spawnFloatingText s c (V2 x y) = do
  Fonts fonts <- get global
  Renderer renderer <- get global
  case renderer of
    Just r ->
      let font = HM.lookup "Assets/Roboto-Regular.ttf" fonts in
        case font of
          Just f -> do
            (tex, size) <- liftIO $ genSolidText r f c s
            void $ newEntity (FloatingTex tex size, Position (V2 (x + ht) y))
          _ -> pure ()
    _ -> pure ()
  where ht = let (V2 t _) = tileSize in fromIntegral t * 0.5

-- Make floating text float up
floatTooltips :: Double -> System' ()
floatTooltips dt = 
  cmapM (\(FloatingTex tex _, Position (V2 x y)) -> 
    if y > (-50) 
       then pure $ Just $ Position (V2 x (y - (dt * 0.1)))
       else do
         liftIO $ SDL.destroyTexture tex
         pure Nothing
  )

-- Get the popup colour based on health left
getDMGPopupColour :: Int -> Int -> SDL.Font.Color
getDMGPopupColour h max 
  | percent > 0.75 = V4 255 255 255 255
  | percent > 0.5 = V4 255 255 0 255
  | percent > 0.25 = V4 255 165 0 255
  | otherwise = V4 255 0 0 255
  where percent = fromIntegral h / fromIntegral max

-- Examine whatever is on the tile at position
examinePos :: V2 Int -> System' ()
examinePos pos = do
  ls :: [(CellRef, Examine)] <- getAll
  case Prelude.lookup (CellRef pos) ls of 
    Just (Examine msg) -> postMessage msg
    _ -> pure ()

-- Type synonym for fonts
type FontFunction = SDL.Font.Color -> Data.Text.Text -> IO SDL.Surface

-- Render text to the screen easily
generateText :: SDL.Renderer -> SDL.Font.Font -> FontFunction -> SDL.Font.Color -> String -> IO (SDL.Texture, V2 Double)
generateText r fo fu c t = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  (w, h) <- SDL.Font.size fo text
  pure (texture, fromIntegral <$> V2 w h)

-- Render solid text
genSolidText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> IO (SDL.Texture, V2 Double)
genSolidText r fo = generateText r fo (SDL.Font.solid fo)

-- Render blended text
genBlendedText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> IO (SDL.Texture, V2 Double)
genBlendedText r fo = generateText r fo (SDL.Font.blended fo)

-- Conversion from Direction to Int V2
directionToVect :: Direction -> V2 Int
directionToVect Common.Up = V2 0 (-1)
directionToVect UpRight = V2 1 (-1)
directionToVect Common.Right = V2 1 0
directionToVect DownRight = V2 1 1
directionToVect Common.Down = V2 0 1
directionToVect DownLeft = V2 (-1) 1
directionToVect Common.Left = V2 (-1) 0
directionToVect UpLeft = V2 (-1) (-1)

-- Conversion from Int V2 to Direction
vectToDirection :: V2 Int -> Maybe Direction
vectToDirection (V2 0 (-1)) = Just Common.Up
vectToDirection (V2 1 (-1)) = Just UpRight
vectToDirection (V2 1 0) = Just Common.Right
vectToDirection (V2 1 1) = Just DownRight
vectToDirection (V2 0 1) = Just Common.Down
vectToDirection (V2 (-1) 1) = Just DownLeft
vectToDirection (V2 (-1) 0) = Just Common.Left
vectToDirection (V2 (-1) (-1)) = Just UpLeft
vectToDirection _ = Nothing

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

tileSize :: Num a => V2 a
tileSize = V2 32 32

standardRange :: Int
standardRange = 2

