{-# LANGUAGE FlexibleContexts #-}

module Draw
( draw
, renderWorld
, renderSprite
, renderReticule
, displayFps
) where

import Apecs
import SDL hiding (get, Vector)
import SDL.Font

import Foreign.C
import Data.HashMap.Strict as HM
import Data.Text(Text, pack)
import Control.Monad(unless, when)
import Control.Arrow((***))

import Data.Matrix
import Data.Vector(ifoldl)

import Common
import Components
import Resources
import GameMap
import Characters


-- Create System' (IO ()) for everything depending on item drawn
draw :: SDL.Renderer -> FPS -> System' ()
draw r fps = do
  Textures texs <- get global
  Fonts fonts <- get global
  let uiFont = HM.lookup "Assets/Roboto-Regular.ttf" fonts
  renderWorld r
  drawComponents $ renderSprite r texs
  drawComponents $ renderReticule r
  drawComponents $ renderFloatingTex r
  displayFps r fps uiFont
  printMessages

-- Produce a system used for drawing
drawComponents :: Get World c => (c -> Position -> System' ()) -> System' ()
drawComponents f = cmapM_ (\(p, comp) -> f comp p)

-- Render the game world simplistically
renderWorld :: SDL.Renderer -> System' ()
renderWorld r = do
  GameMap m <- get global
  rendererDrawColor r $= V4 255 255 255 255
  liftIO $ ifoldl (foldm m) (pure ()) $ getMatrixAsVector m
    where foldm m io i t = let c = ncols m; y = i `div` c; x = i `mod` c; in
            io <> renderTileMessy r (V2 x y) t

-- Render textures
renderSprite :: SDL.Renderer -> TextureMap -> Sprite -> Position -> System' ()
renderSprite r ts (Sprite fp rect) (Position p) = 
  case HM.lookup fp ts of
    Just tex -> 
      liftIO $ SDL.copyEx r tex (Just $ fromIntegral <$> rect) (Just (SDL.Rectangle (P $ round <$> p) tileSize')) 0 Nothing (V2 False False)
    _ -> pure ()

-- Render the target reticule
renderReticule :: SDL.Renderer -> Reticule -> Position -> System' ()
renderReticule r (Reticule on) (Position p) = when on $ do
  rendererDrawColor r $= V4 255 255 255 20
  liftIO $ fillRect r $ Just $ Rectangle (P $ round <$> p) tileSize'

-- Render a tile based on it's type using lines
renderTileMessy :: SDL.Renderer -> V2 Int -> Tile -> IO ()
renderTileMessy r (V2 x y) t =
  let f = fromIntegral
      ti = realToFrac
      (V2 w h) = tileSize'
      (V2 tw th) = V2 (f $ round $ ti w * 0.5) (f $ round $ ti h * 0.5)
      (V2 tx ty) = V2 (f x * w + f (round $ ti w * 0.25)) (f y * h + f (round $ ti h * 0.25)) in
    case t of
      Solid -> do
        drawLine r (P $ V2 tx ty) (P $ V2 (tx + tw) (ty + th))
        drawLine r (P $ V2 (tx + tw) ty) (P $ V2 tx (ty + th))
      _ -> pure ()

-- Display FPS
displayFps :: SDL.Renderer -> Int -> Maybe SDL.Font.Font -> System' ()
displayFps r fps Nothing = pure ()
displayFps r fps (Just f) = do
  (tex, size) <- genSolidText r f (V4 255 255 255 255) ("FPS: " ++ show fps)
  liftIO $ SDL.copy r tex Nothing (Just $ round <$> Rectangle (P $ V2 0 0) size)

-- Render floating text
renderFloatingTex :: SDL.Renderer -> FloatingTex -> Position -> System' ()
renderFloatingTex r (FloatingTex tex size) (Position pos) = 
  liftIO $ SDL.copy r tex Nothing (Just $ round <$> Rectangle (P pos) size)
