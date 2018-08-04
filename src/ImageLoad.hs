module ImageLoad
( getSprite
) where

-- Gloss, graphics processing
import Graphics.Gloss

-- Gloss-Juicy, converting juicy to gloss
import Graphics.Gloss.Juicy

getSprite :: FilePath -> IO (Maybe Picture)
getSprite = loadJuicy
