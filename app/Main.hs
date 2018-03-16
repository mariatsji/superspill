module Main where

import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

data World = World {
    vebjornPos :: (Float, Float)
  , nikolaiPos :: (Float, Float)
  , trollPos  :: (Float, Float)
} deriving (Show)

main :: IO ()
main = playIO
  FullScreen -- Display
  yellow -- Color
  10 -- FPS
  initWorld -- initial world
  paintWorld -- world transformation function
  inputEvent -- (Event -> World -> IO world)
  stepWorld -- (Float -> world -> IO world)

initWorld :: World
initWorld = World {
    trollPos   = (0, 800)
  , vebjornPos = (500, 0)
  , nikolaiPos = (-500, 0)
}

paintWorld :: World -> IO Picture
paintWorld (World vePos niPos trPos) = fmap Pictures $ sequence $
  [ fmap ((Scale 0.5 0.5) . (Translate (fst trPos) (snd trPos))) troll
  , fmap ((Scale 0.5 0.5) . (Translate (fst vePos) (snd vePos))) vebjorn
  , fmap ((Scale 0.5 0.5) . (Translate (fst niPos) (snd niPos))) nikolai ]


picture :: String -> IO Picture
picture t = return
  $ Translate (-170) (-20) -- shift the text to the middle of the window
  $ Scale 0.5 0.5 -- display it half the original size
  $ Text t -- text to display

inputEvent :: Event -> World -> IO World
inputEvent (EventKey (SpecialKey KeySpace) Down modifiers (x,y)) world = return world
--inputEvent (EventKey (Char 't') Down modifiers (x,y)) world = return "TROLL"
inputEvent _ world = return world

stepWorld :: Float -> World -> IO World
stepWorld step world = return world

nikolai :: IO Picture
nikolai = loadBMP "img/nikolai.bmp"

vebjorn :: IO Picture
vebjorn = loadBMP "img/vebjorn.bmp"

troll :: IO Picture
troll = return $ Text "TROLL"