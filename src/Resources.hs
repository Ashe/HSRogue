{-# OPTIONS_GHC -Wall #-}

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
import SDL.Image(loadTexture)
import SDL.Font

import Data.HashMap.Strict

-- Turns a list of key value pairs into a hashmap for a resource component
createResourceMap :: [(String, a)] -> HashMap String a
createResourceMap = foldl (\m (k, v) -> insert k v m) empty

-- Types for creating textures
type TexResource = (String, Texture)
type TextureMap = HashMap String Texture

-- Create a TextureMap with initial filepaths
loadTextures :: Renderer -> [FilePath] -> IO [TexResource]
loadTextures r = traverse getTex
  where getTex p = do
          tex <- loadTexture r p
          pure (p, tex)

-- Types for creating fonts
type FontResource = (String, Font)
type FontMap = HashMap String Font

-- Create a FontMap using initial filepaths
loadFonts :: [(FilePath, PointSize)] -> IO [FontResource]
loadFonts = traverse getFont
  where getFont (p, s) = do
          font <- load p s
          pure (p, font)
