module Resources
( createResourceMap
, TexResource
, TextureMap
, loadTextures
, FontResource
, FontMap
, loadFonts
) where

import SDL(Renderer, Texture)
import SDL.Vect
import SDL.Image(loadTexture)
import SDL.Font

import Data.HashMap as HM
import Control.Monad.IO.Class

-- Turns a list of key value pairs into a hashamp for a resource component
createResourceMap :: [(String, a)] -> HM.Map String a
createResourceMap = foldl (\map (k, v) -> insert k v map) empty 

-- Types for creating textures
type TexResource = (String, Texture)
type TextureMap = HM.Map String Texture

-- Create a TextureMap with initial filepaths
loadTextures :: Renderer -> [FilePath] -> IO [TexResource]
loadTextures r = traverse getTex
  where getTex p = do
          tex <- loadTexture r p
          pure (p, tex)

-- Types for creating fonts
type FontResource = (String, Font)
type FontMap = HM.Map String Font

-- Create a FontMap using initial filepaths
loadFonts :: [(FilePath, PointSize)] -> IO [FontResource]
loadFonts = traverse getFont
  where getFont (p, s) = do
          font <- load p s
          pure (p, font)
