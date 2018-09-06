{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Common
( World(..)
, initWorld
, System'
, Direction(..)
, CharacterList
, postMessage
, printMessages
, clearMessages
, actionStep
, directionToVect
, toCIntRect
, toCIntV2
, renderWorld
, renderSprite
, renderReticule
, renderSolidText
, renderBlendedText
, displayFps
, worldScale
, playerPos
, playerCellRef
, tileSize
) where

import Apecs
import SDL hiding (get, Vector)
import SDL.Font
import Foreign.C
import Data.HashMap as HM
import Data.Text(Text, pack)
import Control.Arrow((***))

import Data.Vector (ifoldl)

import Components
import Resources
import GameMap
import Characters

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''Messages, ''GameState, ''Textures, ''Fonts, ''GameMap, 
  ''Player, ''Reticule, ''Position, ''CellRef, ''Sprite, ''Character, ''Examine] 

-- Easy type synonym for systems
type System' a = System World a

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

-- System called whenever the player performs a proper action
actionStep :: System' ()
actionStep = do
  writeExamines
  pure ()

-- Display FPS
displayFps :: SDL.Renderer -> Int -> FontMap -> String -> System' (IO ())
displayFps r fps fontMap fp = 
  case HM.lookup fp fontMap of 
    Just f -> pure $ renderSolidText r f (V4 255 255 255 255) ("FPS: " ++ show fps) (V2 0 0)
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

-- Render the game world simplistically
renderWorld :: SDL.Renderer -> System' (IO ())
renderWorld r = do
  GameMap m <- get global
  rendererDrawColor r $= V4 255 255 255 255
  pure $ ifoldl foldRow (pure ()) m
    where foldRow io y row = io <> ifoldl (foldidx y) (pure ()) row
          foldidx y io x t = io <> renderTileMessy r (V2 x y) t

-- Render textures
renderSprite :: SDL.Renderer -> TextureMap -> Sprite -> Position -> IO ()
renderSprite r ts (Sprite fp rect) (Position p) = 
  case HM.lookup fp ts of
    Just tex -> SDL.copyEx r tex (Just $ toCIntRect rect) (Just (SDL.Rectangle (P $ toCIntV2 p) tileSize)) 0 Nothing (V2 False False)
    _ -> pure ()

-- Render the target reticule
renderReticule :: SDL.Renderer -> Reticule -> Position -> IO ()
renderReticule r (Reticule on) (Position p)
  | not on = pure ()
  | on = do
    rendererDrawColor r $= V4 255 255 255 20
    fillRect r $ Just $ Rectangle (P $ toCIntV2 p) tileSize

-- Render a tile based on it's type using lines
renderTileMessy :: SDL.Renderer -> V2 Int -> Tile -> IO ()
renderTileMessy r (V2 x y) t =
  let f = fromIntegral
      ti = realToFrac
      (V2 w h) = tileSize 
      (V2 tw th) = V2 (f $ round $ ti w * 0.5) (f $ round $ ti h * 0.5)
      (V2 tx ty) = V2 (f x * w + f (round $ ti w * 0.25)) (f y * h + f (round $ ti h * 0.25)) in
    case t of
      Solid -> do
        drawLine r (P $ V2 tx ty) (P $ V2 (tx + tw) (ty + th))
        drawLine r (P $ V2 (tx + tw) ty) (P $ V2 tx (ty + th))
      _ -> pure ()
        
-- Render text to the screen easily
renderText :: SDL.Renderer -> SDL.Font.Font -> (SDL.Font.Color -> Data.Text.Text -> IO SDL.Surface) ->
           SDL.Font.Color -> String -> V2 Int -> IO ()
renderText r fo fu c t (V2 x y) = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  fontSize <- SDL.Font.size fo text
  let (w, h) = (fromIntegral *** fromIntegral) fontSize
      x' = fromIntegral x
      y' = fromIntegral y
  SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x' y') (V2 w h)))
  SDL.destroyTexture texture

-- Render solid text
renderSolidText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> V2 Int -> IO ()
renderSolidText r fo = renderText r fo (SDL.Font.solid fo)

-- Render blended text
renderBlendedText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> V2 Int -> IO ()
renderBlendedText r fo = renderText r fo (SDL.Font.blended fo)

-- Place important information into examine messages
writeExamines :: System' ()
writeExamines = 
  cmap(\(Character n h mH a) -> Examine $ 
    n ++ ": " ++ show a ++ ", Health: " ++ show h ++ "/" ++ show mH)

worldScale :: Double
worldScale = 32

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

tileSize :: V2 CInt
tileSize = V2 32 32

