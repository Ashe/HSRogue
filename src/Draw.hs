{-# LANGUAGE FlexibleContexts #-}

module Draw
( draw
) where

import Apecs
import SDL hiding (get, Vector)
import SDL.Font

import Foreign.C
import Data.HashMap.Strict as HM
import Data.Text(Text, pack)
import Control.Monad(unless, when, void)
import Control.Monad.IO.Class(MonadIO)

import Data.Matrix
import Data.Vector(ifoldl)

import Types
import Common
import Components
import Resources
import GameMap
import Characters

-- Draw everything in the game
draw :: SDL.Renderer -> FPS -> V2 CInt -> System' ()
draw r fps ws@(V2 gsw gsh) = do
  Fonts fonts <- get global
  let V2 wsw _ = let V2 tsw tsh = tileSize in V2 (40 * tsw) (32 * tsh)
  drawGameWorld r
  drawGameUI r fonts $ fromIntegral <$> Rectangle (P $ V2 wsw 0) (V2 (gsw - wsw) gsh)
  drawGameOverlay r fonts fps ws

-- Render the game map as well as any entities in game
drawGameWorld :: SDL.Renderer -> System' ()
drawGameWorld r = do
  Textures texs <- get global
  renderWorld r
  drawComponents $ renderSprite r texs
  drawComponents $ renderReticule r
  drawComponents $ renderFloatingTex r

-- Draw the UI within bounds
drawGameUI :: SDL.Renderer -> FontMap -> SDL.Rectangle CInt -> System' ()
drawGameUI r fonts bounds@(SDL.Rectangle (P p@(V2 x y)) d@(V2 w h)) = do
  let uiFont = HM.lookup "Assets/Roboto-Regular.ttf" fonts
  SDL.drawLine r (P p) (P $ p + V2 0 h)
  displayMessages r bounds uiFont

-- Display anything that goes over the main game
drawGameOverlay :: SDL.Renderer -> FontMap -> FPS -> V2 CInt -> System' ()
drawGameOverlay r fonts fps ws = do
  let uiFont = HM.lookup "Assets/Roboto-Regular.ttf" fonts
  GameState state <- get global
  case state of
    Interface PauseScreen ->
      drawPrompt r ws (V2 650 300)
    _ -> pure ()
  displayFps r fps uiFont

-- Produce a system used for drawing
drawComponents :: Get World c => (c -> Position -> IO ()) -> System' ()
drawComponents f = cmapM_ (\(p, comp) -> liftIO $ f comp p)

-- Render the game world simplistically
renderWorld :: SDL.Renderer -> System' ()
renderWorld r = do
  GameMap m <- get global
  rendererDrawColor r $= V4 255 255 255 255
  liftIO $ ifoldl (foldm m) (pure ()) $ getMatrixAsVector m
    where foldm m io i t = let c = ncols m; y = i `div` c; x = i `mod` c; in
            io <> renderTileMessy r (V2 x y) t

-- Render textures
renderSprite :: SDL.Renderer -> TextureMap -> Sprite -> Position -> IO ()
renderSprite r ts (Sprite fp rect) (Position p) = 
  case HM.lookup fp ts of
    Just tex -> 
      SDL.copyEx r tex (Just $ fromIntegral <$> rect) (Just (SDL.Rectangle (P $ round <$> p) tileSize)) 0 Nothing (V2 False False)
    _ -> pure ()

-- Render the target reticule
renderReticule :: SDL.Renderer -> Reticule -> Position -> IO ()
renderReticule r (Reticule on) (Position p) = when on $ do
  rendererDrawColor r $= V4 255 255 255 20
  fillRect r $ Just $ Rectangle (P $ round <$> p) tileSize

-- Render a tile based on it's type using lines
renderTileMessy :: SDL.Renderer -> V2 Int -> Tile -> IO ()
renderTileMessy r (V2 x y) Solid = do
  let f = fromIntegral
      ti = realToFrac
      (V2 w h) = tileSize
      (V2 tw th) = V2 (f $ round $ ti w * 0.5) (f $ round $ ti h * 0.5)
      (V2 tx ty) = V2 (f x * w + f (round $ ti w * 0.25)) (f y * h + f (round $ ti h * 0.25))
  drawLine r (P $ V2 tx ty) (P $ V2 (tx + tw) (ty + th))
  drawLine r (P $ V2 (tx + tw) ty) (P $ V2 tx (ty + th))
renderTileMessy r _ _ = pure ()

-- Display FPS
displayFps :: SDL.Renderer -> Int -> Maybe SDL.Font.Font -> System' ()
displayFps r fps Nothing = pure ()
displayFps r fps (Just f) = do
  (tex, size) <- genSolidText r f (V4 255 255 255 255) ("FPS: " ++ show fps)
  SDL.copy r tex Nothing (Just $ round <$> Rectangle (P $ V2 0 0) size)
  SDL.destroyTexture tex

-- Render floating text
renderFloatingTex :: SDL.Renderer -> FloatingTex -> Position -> IO ()
renderFloatingTex r (FloatingTex tex size) (Position pos) = 
 SDL.copy r tex Nothing (Just $ round <$> Rectangle (P pos) size)

-- Display all messages
displayMessages :: SDL.Renderer -> SDL.Rectangle CInt -> Maybe SDL.Font.Font -> System' ()
displayMessages _ _ Nothing = pure ()
displayMessages r (SDL.Rectangle (P anchor) (V2 w h)) (Just f) = do
  Messages messages <- get global
  let msgs = zip messages [0..]
  mapM_ (\(msg, index) -> do
    (tex, size) <- liftIO $ genMessage r f msg
    SDL.copy r tex Nothing (Just $ Rectangle (P $ anchor + V2 2 (index * 14)) (round <$> size))
    SDL.destroyTexture tex) msgs

-- Draw prompt in the centre of the screen of the selected size
drawPrompt :: SDL.Renderer -> V2 CInt -> V2 CInt -> System' ()
drawPrompt r win dim = do
  let topLeft = P $ fmap (`div` 2) win - fmap (`div` 2) dim
  rendererDrawColor r $= V4 0 0 0 0
  SDL.fillRect r $ Just $ SDL.Rectangle topLeft dim
  rendererDrawColor r $= V4 255 255 255 255
  SDL.drawRect r $ Just $ SDL.Rectangle topLeft dim
