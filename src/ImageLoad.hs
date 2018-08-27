module ImageLoad
( Resources
, loadTextures
, createTextureComp
) where

import SDL(Renderer, Texture)
import SDL.Vect
import SDL.Image(loadTexture)
import Data.HashMap as HM
import Control.Monad.IO.Class

import Components

-- Easy type for creating resources for 
type Resources = [(String, Texture)]

-- Create a Textures component  with initial filepaths
loadTextures :: Renderer -> [FilePath] -> IO Resources
loadTextures r = traverse getTex
  where getTex p = do
          tex <- loadTexture r p
          pure (p, tex)

-- Turns a list of key value pairs into a hashamp for the texture component
createTextureComp :: Resources -> Textures
createTextureComp kvps = Textures $ foldl (\map (k, v) -> insert k v map) empty kvps
