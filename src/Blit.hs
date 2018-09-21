module Blit
( draw
, blankSurface
) where

import Apecs
import SDL hiding (get, Vector)
import SDL.Font

import Foreign.C
import Data.HashMap as HM
import Data.Text(Text, pack)
import Control.Monad(unless, when, void)
import Control.Monad.IO.Class(MonadIO)
import Control.Arrow((***))

import Data.Vector (ifoldl)

import Common
import Components
import Resources

-- Type synonym for fonts
type FontFunction = SDL.Font.Color -> Data.Text.Text -> IO SDL.Surface

-- Handle surface drawing
draw :: SDL.Renderer -> FPS -> TextureMap -> System' (IO ())
draw r fps texs = do
  WindowSize ws@(V2 w h) <- get global
  Fonts fonts <- get global
  UISurface surface <- get global
  let uiFont = HM.lookup "Assets/Roboto-Regular.ttf" fonts
  case surface of
    Just s -> 
      when (w > 0 && h > 0) $ do
        surfaceFillRect s Nothing (V4 0 0 0 0)
        -- displayFps s r fps uiFont
        renderSurface r s
    _ ->
      pure ()

-- Easy way to create a blank surface, MUST be destroyed
blankSurface :: MonadIO m => V2 Int -> m Surface
blankSurface s = masksToPixelFormat 32 (V4 0 0 0 0) >>= createRGBSurface (fromIntegral <$> s)

-- Display FPS
displayFps :: SDL.Surface -> SDL.Renderer -> Int -> Maybe SDL.Font.Font -> System' ()
displayFps s r fps Nothing = pure ()
displayFps s r fps (Just f) = 
  liftIO $ blitSolidText s r f (V4 255 255 255 255) ("FPS: " ++ show fps) (V2 0 0) False

-- Render a given surface by turning it into a texture
renderSurface :: MonadIO m => SDL.Renderer -> Surface -> m ()
renderSurface r s = do
  texture <- createTextureFromSurface r s
  SDL.copy r texture Nothing Nothing
  SDL.destroyTexture texture

-- Render solid text
blitSolidText :: SDL.Surface -> SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> V2 Double -> Bool -> IO ()
blitSolidText target r fo c str p = blitText target r fo (SDL.Font.solid fo) c str (toCIntV2 p)

-- Blit text to a surface
blitText :: SDL.Surface -> SDL.Renderer -> SDL.Font.Font -> FontFunction -> SDL.Font.Color -> String -> V2 CInt -> Bool -> IO ()
blitText target r fo fu c t (V2 x y) center = do
  let text = Data.Text.pack t
  surface <- fu c text
  fontSize <- SDL.Font.size fo text
  void $ if center 
    then 
      let x' = x - fromIntegral (fst fontSize `div` 2) in
      SDL.surfaceBlit surface Nothing target $ Just (P $ V2 x' y)
    else
      SDL.surfaceBlit surface Nothing target $ Just (P $ V2 x y)
  SDL.freeSurface surface

