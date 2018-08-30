{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common
( World(..)
, initWorld
, System'
, Direction(..)
, postMessage
, printMessages
, clearMessages
, directionToVect
, toCIntRect
, toCIntV2
, renderSprite
, renderSolidText
, renderBlendedText
, displayFps
, worldScale
, playerPos
, playerCellRef
, playerSpeed
, tileSize
) where

import Apecs
import SDL hiding (get)
import SDL.Font
import Foreign.C
import Data.HashMap as HM
import Data.Text(Text, pack)
import Control.Arrow((***))

import Components
import Resources

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''Messages, ''Textures, ''Fonts, ''GameMap, ''Player, ''Position, ''CellRef, ''Sprite] 

-- Easy type synonym for systems
type System' a = System World a

-- Directions
data Direction = 
  Up | UpRight | Right | DownRight | Down | DownLeft | Left | UpLeft
  deriving Show

-- Post a new message
postMessage :: String -> System' ()
postMessage m = modify global (\(Messages msgs) -> Messages $ m : msgs)

-- Print messages into console
printMessages :: System' (IO ())
printMessages = do
  Messages msgs <- get global
  pure $ foldl (\io m ->io <> print m) mempty msgs

-- Flush any messages
clearMessages :: System' ()
clearMessages = modify global (\(Messages _) -> Messages [])

-- Display FPS
displayFps :: SDL.Renderer -> FontMap -> String -> System' (IO ())
displayFps r fontMap fp = do
  Time time <- get global
  case HM.lookup fp fontMap of 
    Just f -> pure $ renderSolidText r f (V4 255 255 255 255) (show time) 0 0
    _ -> pure $ pure ()

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

-- Conversion from Int Rectangle to CInt Rectangle
toCIntRect :: Rectangle Int -> Rectangle CInt
toCIntRect (Rectangle (P (V2 x y)) (V2 i j)) = 
  Rectangle (P (V2 (fromIntegral x) (fromIntegral y))) (V2 (fromIntegral i) (fromIntegral j))

-- Conversion from Int Vector to CInt Vector
toCIntV2 :: V2 Double -> V2 CInt
toCIntV2 (V2 x y) = V2 (round x) (round y)

-- Render textures
renderSprite :: SDL.Renderer -> TextureMap -> Sprite -> Position -> IO ()
renderSprite r ts (Sprite fp rect) (Position p) = 
  case HM.lookup fp ts of
    Just tex -> SDL.copyEx r tex (Just $ toCIntRect rect) (Just (SDL.Rectangle (P $ toCIntV2 p) tileSize)) 0 Nothing (V2 False False)
    _ -> pure ()

-- Render text to the screen easily
renderText :: SDL.Renderer -> SDL.Font.Font -> (SDL.Font.Color -> Data.Text.Text -> IO SDL.Surface) ->
           SDL.Font.Color -> String -> Int -> Int -> IO ()
renderText r fo fu c t x y = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  size <- SDL.Font.size fo text
  let (w, h) = (fromIntegral *** fromIntegral) size
      x' = fromIntegral x
      y' = fromIntegral y
  SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x' y') (V2 w h)))
  SDL.destroyTexture texture

-- Render solid text
renderSolidText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> Int -> Int -> IO ()
renderSolidText r fo = renderText r fo (SDL.Font.solid fo)

-- Render blended text
renderBlendedText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> Int -> Int -> IO ()
renderBlendedText r fo = renderText r fo (SDL.Font.blended fo)

worldScale :: Double
worldScale = 32

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

playerSpeed :: Int
playerSpeed = 1

tileSize :: V2 CInt
tileSize = V2 32 32

