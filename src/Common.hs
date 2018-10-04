{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common
( World(..)
, initWorld
, System'
, CharacterList
, postMessage
, clearMessages
, snapEntities
, spawnFloatingText
, floatTooltips
, getHealthColour
, examinePos
, genSolidText
, genBlendedText
, getTextFromMessage
, genMessage

, directionToVect
, vectToDirection
, playerPos
, playerCellRef
, tileSize
, standardRange
) where

import Apecs
import SDL hiding (get, Vector, Renderer)
import qualified SDL
import SDL.Font
import Foreign.C
import Data.HashMap.Strict as HM
import Data.Text(Text, pack)
import Control.Monad(void, foldM_)
import Control.Monad.IO.Class (MonadIO)

import Data.Vector (ifoldl)

import Types as T
import Components
import Resources
import Characters

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''WindowSize, ''Messages, ''GameState, ''Textures, ''Renderer,''Fonts
                  , ''GameMap , ''Player, ''PlayerReady, ''PlayerPath, ''Relationships, ''Reticule
                  , ''Position , ''CellRef, ''Sprite , ''Character , ''Examine, ''FloatingTex] 

-- Easy type synonym for systems
type System' a = System World a

-- Easy way of getting all non-player entities
type CharacterList = [(Character, CellRef, Entity)]

-- Post a new message
postMessage :: [MBit] -> System' ()
postMessage m = do
  liftIO $ putStrLn $ getTextFromMessage m
  modify global (\(Messages msgs) -> Messages $ m : msgs)

-- Flush any messages
clearMessages :: System' ()
clearMessages = modify global (\(Messages msgs) -> Messages $ take 45 msgs)

-- Converts cell references to game position
snapEntities :: System' ()
snapEntities = cmap (\(Position (V2 _ _), CellRef p) ->
  Position $ fromIntegral <$> p * tileSize)

-- Spawn a floating tooltip
spawnFloatingText :: String -> SDL.Font.Color -> V2 Double -> System' ()
spawnFloatingText s c (V2 x y) = do
  Fonts fonts <- get global
  Renderer renderer <- get global
  case renderer of
    Just r ->
      let font = HM.lookup "Assets/Roboto-Regular.ttf" fonts in
        case font of
          Just f -> do
            (tex, size) <- genSolidText r f c s
            let ht = let (V2 t _) = tileSize in fromIntegral t * 0.5
                center = let V2 w _ = size in x + ht - (w / 2)
            void $ newEntity (FloatingTex tex size, Position (V2 center y))
          _ -> pure ()
    _ -> pure ()

-- Make floating text float up
floatTooltips :: Double -> System' ()
floatTooltips dt = 
  cmapM (\(FloatingTex tex _, Position (V2 x y)) -> 
    if y > (-50) 
       then pure $ Just $ Position (V2 x (y - (dt * 0.1)))
       else do
         SDL.destroyTexture tex
         pure Nothing
  )

-- Get the popup colour based on health left
getHealthColour :: Int -> Int -> SDL.Font.Color
getHealthColour h max 
  | percent > 0.75 = V4 255 255 255 255
  | percent > 0.5 = V4 255 255 0 255
  | percent > 0.25 = V4 255 165 0 255
  | otherwise = V4 255 0 0 255
  where percent = fromIntegral h / fromIntegral max


-- Examine whatever is on the tile at position
examinePos :: V2 Int -> System' ()
examinePos pos = do
  ls :: [(CellRef, Examine)] <- getAll
  case Prelude.lookup (CellRef pos) ls of 
    Just (Examine msg) -> postMessage msg
    _ -> pure ()

-- Render text to the screen easily
generateText :: MonadIO m => SDL.Renderer -> SDL.Font.Font 
             -> (SDL.Font.Color -> Data.Text.Text -> m SDL.Surface)
             -> SDL.Font.Color -> String -> m (SDL.Texture, V2 Double)
generateText r fo fu c t = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  (w, h) <- SDL.Font.size fo text
  pure (texture, fromIntegral <$> V2 w h)

-- Render solid text
genSolidText :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> m (SDL.Texture, V2 Double)
genSolidText r fo = generateText r fo (SDL.Font.solid fo)

-- Render blended text
genBlendedText :: MonadIO m => SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> m (SDL.Texture, V2 Double)
genBlendedText r fo = generateText r fo (SDL.Font.blended fo)

-- Easy way of getting text from a message
getTextFromMessage :: [MBit] -> String
getTextFromMessage = foldl (\t (MBit n) -> let (txt,_) = render n in t ++ txt) ""

-- Generate a message to display
genMessage :: SDL.Renderer -> SDL.Font.Font -> [MBit] -> IO (SDL.Texture, V2 Double)
genMessage r f m = do
  let plainText = getTextFromMessage m
  (width, height) <- SDL.Font.size f $ Data.Text.pack plainText
  let size = fromIntegral <$> V2 width height
  pixelformat <- SDL.masksToPixelFormat 16 (V4 0 0 0 0)
  surface <- SDL.createRGBSurface size pixelformat

  foldM_ (\p (MBit next) -> do
    let (txt, col) = render next
        text = Data.Text.pack txt
    (bitw, _) <- SDL.Font.size f text
    txtSurface <- SDL.Font.solid f col text
    SDL.surfaceBlit txtSurface Nothing surface (Just $ P (V2 p 0))
    SDL.freeSurface txtSurface
    pure $ p + fromIntegral bitw
    ) 0 m

  tex <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  pure (tex, fromIntegral <$> size)

-- Conversion from Direction to Int V2
directionToVect :: Direction -> V2 Int
directionToVect T.Up = V2 0 (-1)
directionToVect UpRight = V2 1 (-1)
directionToVect T.Right = V2 1 0
directionToVect DownRight = V2 1 1
directionToVect T.Down = V2 0 1
directionToVect DownLeft = V2 (-1) 1
directionToVect T.Left = V2 (-1) 0
directionToVect UpLeft = V2 (-1) (-1)

-- Conversion from Int V2 to Direction
vectToDirection :: V2 Int -> Maybe Direction
vectToDirection (V2 0 (-1)) = Just T.Up
vectToDirection (V2 1 (-1)) = Just UpRight
vectToDirection (V2 1 0) = Just T.Right
vectToDirection (V2 1 1) = Just DownRight
vectToDirection (V2 0 1) = Just T.Down
vectToDirection (V2 (-1) 1) = Just DownLeft
vectToDirection (V2 (-1) 0) = Just T.Left
vectToDirection (V2 (-1) (-1)) = Just UpLeft
vectToDirection _ = Nothing

playerPos :: V2 Double
playerPos = V2 0 0

playerCellRef :: V2 Int
playerCellRef = V2 0 0

tileSize :: Num a => V2 a
tileSize = V2 16 16

standardRange :: Int
standardRange = 2

