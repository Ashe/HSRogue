module Resources
( createResourceMap
, loadTextures
, loadFonts
) where

import SDL(Renderer)
import SDL.Image(loadTexture)
import SDL.Font
import Data.HashMap.Strict

import Types

-- Turns a list of key value pairs into a hashmap for a resource component
createResourceMap :: [(String, a)] -> HashMap String a
createResourceMap = foldl (\m (k, v) -> insert k v m) empty

-- Create a TextureMap with initial filepaths
loadTextures :: Renderer -> [FilePath] -> IO [TexResource]
loadTextures r = traverse getTex
  where getTex p = do
          tex <- loadTexture r p
          pure (p, tex)

-- Create a FontMap using initial filepaths
loadFonts :: [(FilePath, PointSize)] -> IO [FontResource]
loadFonts = traverse getFont
  where getFont (p, s) = do
          font <- load p s
          pure (p, font)
