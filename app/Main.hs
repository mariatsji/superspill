module Main where

import Control.Monad

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import System.Random

type Pos = (Float, Float)

data World = World {
    vebjornPos :: Pos
  , nikolaiPos :: Pos
  , trollPos  :: Pos
  , trollDst :: Pos
} deriving (Show)

main :: IO ()
main = do
  trDst' <- rndDst
  playIO
    FullScreen -- Display
    yellow -- Color
    10 -- FPS
    (initWorld trDst') -- initial world
    paintWorld -- world transformation function
    inputEvent -- (Event -> World -> IO world)
    stepWorld -- (Float -> world -> IO world)

rndDst :: IO Pos
rndDst = do
  rndTrollDstX <- randomIO :: IO Float
  rndTrollDstY <- randomIO :: IO Float
  return ((rndTrollDstX - 0.5) * 1000, (rndTrollDstY - 0.5) * 1000)

initWorld :: Pos -> World
initWorld dst = World {
    trollPos   = (0, 10000)
  , vebjornPos = (500, 10000)
  , nikolaiPos = (-500, 10000)
  , trollDst = dst
}

paintWorld :: World -> IO Picture
paintWorld (World vePos niPos trPos trDst) = do
  print $ "printing troll at " ++ show trPos ++ " towards " ++ show trDst
  fmap Pictures $ sequence $
    [ fmap ((Scale 0.5 0.5) . (Translate (fst trPos) (snd trPos))) troll
    , fmap ((Scale 0.5 0.5) . (Translate (fst vePos) (snd vePos))) vebjorn
    , fmap ((Scale 0.5 0.5) . (Translate (fst niPos) (snd niPos))) nikolai ]


picture :: String -> IO Picture
picture t = return
  $ Translate (-170) (-20) -- shift the text to the middle of the window
  $ Scale 0.5 0.5 -- display it half the original size
  $ Text t -- text to display

inputEvent :: Event -> World -> IO World
--inputEvent (EventKey (SpecialKey KeySpace) Down modifiers (x,y)) world = return world
inputEvent (EventKey (Char 't') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World vePos niPos (0, 800) trDst)
inputEvent (EventKey (Char 'n') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World vePos (-500, 0) trPos trDst)
inputEvent (EventKey (Char 'v') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World (500, 0) niPos trPos trDst)
inputEvent (EventKey (SpecialKey KeyUp) Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World (add (0, 20) vePos) niPos trPos trDst)
inputEvent (EventKey (SpecialKey KeyDown) Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World (add (0, -20) vePos) niPos trPos trDst)
inputEvent (EventKey (SpecialKey KeyLeft) Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World (add (-20, 0) vePos) niPos trPos trDst)
inputEvent (EventKey (SpecialKey KeyRight) Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World (add (20, 0) vePos) niPos trPos trDst)
inputEvent (EventKey (Char 'w') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World vePos (add (0, 20) niPos) trPos trDst)
inputEvent (EventKey (Char 's') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World vePos (add (0, -20) niPos) trPos trDst)
inputEvent (EventKey (Char 'a') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World vePos (add (-20, 0) niPos) trPos trDst)
inputEvent (EventKey (Char 'd') Down modifiers (x,y)) (World vePos niPos trPos trDst) = return (World vePos (add (20, 0) niPos) trPos trDst)
inputEvent _ world = return world

stepWorld :: Float -> World -> IO World
stepWorld step (World vePos niPos trPos trDst) = do
  if trPos &= trDst then do
    newTrDst <- rndDst
    return (World vePos niPos trPos newTrDst)
  else
    return (World vePos niPos (nextPos step trPos trDst) trDst)

nextPos :: Float -> Pos -> Pos -> Pos
nextPos step currPos@(x',y') destPos@(x,y)
  | x > x', y > y' = (x' + trollSpeed, y' + trollSpeed)
  | x > x', y <= y' = (x' + trollSpeed, y' - trollSpeed)
  | x <= x', y > y' = (x' - trollSpeed, y' + trollSpeed)
  | x <= x', y <= y' = (x' - trollSpeed, y' - trollSpeed)
    where trollSpeed = 2

-- fuzzy close match
(&=) :: Pos -> Pos -> Bool
(&=) (x1,y1) (x2,y2) = abs (x2 - x1) < 10 && abs (y2 - y1) < 10

add :: Pos -> Pos -> Pos
add (x,y) (a, b) = (x + a, y + b)

nikolai :: IO Picture
nikolai = loadBMP "img/nikolai.bmp"

vebjorn :: IO Picture
vebjorn = loadBMP "img/vebjorn.bmp"

troll :: IO Picture
troll = loadBMP "img/troll.bmp"

