module Main where

import           Control.Monad

import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game

import           System.Random

type Pos = (Float, Float)

data World = World
  { vebjornPos :: Pos
  , nikolaiPos :: Pos
  , trollPos   :: Pos
  , trollDst   :: Pos
  , trollImg   :: Int
  , stange     :: Int
  } deriving (Show)

fps :: Int
fps = 10

main :: IO ()
main = do
  trDst' <- rndDst
  playIO
    FullScreen -- Display
    yellow -- Color
    fps -- FPS
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
initWorld dst =
  World
  { trollPos = (0, 3000)
  , vebjornPos = (500, 10000)
  , nikolaiPos = (-500, 10000)
  , trollDst = dst
  , trollImg = 1
  , stange = 0
  }

paintWorld :: World -> IO Picture
paintWorld (World vePos niPos trPos trDst trImg stange)
  | stange > 0 =
    fmap Pictures $
    sequence $
    [ fmap
        ((Scale 0.5 0.5) . (Translate (fst trPos) (snd trPos)))
        (troll' trImg)
    , fmap ((Scale 0.5 0.5) . (Translate (fst vePos) (snd vePos))) vebjorn
    , fmap ((Scale 0.5 0.5) . (Translate (fst niPos) (snd niPos))) nikolai
    , fmap ((Scale 1 1) . (Translate 0 0)) stange'
    ]
  | otherwise =
    fmap Pictures $
    sequence $
    [ fmap
        ((Scale 0.5 0.5) . (Translate (fst trPos) (snd trPos)))
        (troll' trImg)
    , fmap ((Scale 0.5 0.5) . (Translate (fst vePos) (snd vePos))) vebjorn
    , fmap ((Scale 0.5 0.5) . (Translate (fst niPos) (snd niPos))) nikolai
    ]

inputEvent :: Event -> World -> IO World
--inputEvent (EventKey (SpecialKey KeySpace) Down modifiers (x,y)) world = return world
inputEvent (EventKey (Char 't') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos niPos (0, 800) trDst 1 stange)
inputEvent (EventKey (Char 'n') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos (-500, 0) trPos trDst trImg stange)
inputEvent (EventKey (Char 'v') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World (500, 0) niPos trPos trDst trImg stange)
inputEvent (EventKey (SpecialKey KeyUp) Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World (add (0, 25) vePos) niPos trPos trDst trImg stange)
inputEvent (EventKey (SpecialKey KeyDown) Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World (add (0, -25) vePos) niPos trPos trDst trImg stange)
inputEvent (EventKey (SpecialKey KeyLeft) Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World (add (-25, 0) vePos) niPos trPos trDst trImg stange)
inputEvent (EventKey (SpecialKey KeyRight) Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World (add (25, 0) vePos) niPos trPos trDst trImg stange)
inputEvent (EventKey (Char 'w') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos (add (0, 25) niPos) trPos trDst trImg stange)
inputEvent (EventKey (Char 's') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos (add (0, -25) niPos) trPos trDst trImg stange)
inputEvent (EventKey (Char 'a') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos (add (-25, 0) niPos) trPos trDst trImg stange)
inputEvent (EventKey (Char 'd') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos (add (25, 0) niPos) trPos trDst trImg stange)
inputEvent (EventKey (SpecialKey KeySpace) Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (World vePos niPos trPos trDst 50 10)
inputEvent (EventKey (Char 'p') Down modifiers (x, y)) (World vePos niPos trPos trDst trImg stange) =
  return (initWorld (0, 3000))
inputEvent _ world = return world

stepWorld :: Float -> World -> IO World
stepWorld step (World vePos niPos trPos trDst trImg stange) = do
  if trPos &= trDst
    then do
      newTrDst <- rndDst
      return (World vePos niPos trPos newTrDst (trImg - 1) (stange - 1))
    else return
           (World
              vePos
              niPos
              (nextPos step trPos trDst)
              trDst
              (trImg - 1)
              (stange - 1))

-- fuzzy close match
(&=) :: Pos -> Pos -> Bool
(&=) (x1, y1) (x2, y2) = abs (x2 - x1) < 10 && abs (y2 - y1) < 10

nextPos :: Float -> Pos -> Pos -> Pos
nextPos step currPos@(x', y') destPos@(x, y)
  | x > x'
  , y > y' = (x' + trollSpeed, y' + trollSpeed)
  | x > x'
  , y <= y' = (x' + trollSpeed, y' - trollSpeed)
  | x <= x'
  , y > y' = (x' - trollSpeed, y' + trollSpeed)
  | x <= x'
  , y <= y' = (x' - trollSpeed, y' - trollSpeed)
  where
    trollSpeed = 2

add :: Pos -> Pos -> Pos
add (x, y) (a, b) = (x + a, y + b)

nikolai :: IO Picture
nikolai = loadBMP "img/nikolai.bmp"

vebjorn :: IO Picture
vebjorn = loadBMP "img/vebjorn.bmp"

troll' :: Int -> IO Picture
troll' i
  | i > 0 = troll2
  | otherwise = troll

troll :: IO Picture
troll = loadBMP "img/troll.bmp"

troll2 :: IO Picture
troll2 = loadBMP "img/troll_redd.bmp"

stange' :: IO Picture
stange' = loadBMP "img/stange.bmp"
