{-# LANGUAGE FlexibleContexts #-}

module Draw
( draw
, renderWorld
, renderSprite
, renderReticule
, renderSolidText
, renderBlendedText
, displayFps
) where

import Apecs
import SDL hiding (get, Vector)
import SDL.Font

import Data.HashMap as HM
import Data.Text(Text, pack)
import Control.Arrow((***))

import Data.Vector (ifoldl)

import Common
import Components
import Resources
import GameMap
import Characters

-- Create System' (IO ()) for everything depending on item drawn
draw :: SDL.Renderer -> Int -> System' (IO ())
draw renderer fps = do
  Textures texs <- get global
  Fonts fonts <- get global
  sequence_ <$> sequence 
    [ renderWorld renderer
    , drawComponents $ renderSprite renderer texs
    , drawComponents $ renderReticule renderer
    , printMessages
    , displayFps renderer fps fonts "Assets/Roboto-Regular.ttf"
    ]

-- Produce a system used for drawing
drawComponents :: Get World c => (c -> Position -> IO ()) -> System' (IO ())
drawComponents f = cfold (\img (p, comp) -> img <> f comp p) mempty

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

-- Display FPS
displayFps :: SDL.Renderer -> Int -> FontMap -> String -> System' (IO ())
displayFps r fps fontMap fp = 
  case HM.lookup fp fontMap of 
    Just f -> pure $ renderSolidText r f (V4 255 255 255 255) ("FPS: " ++ show fps) (V2 0 0)
    _ -> pure $ pure ()

