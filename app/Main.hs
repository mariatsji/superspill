module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

import Lib

type World = String

main :: IO ()
main = playIO
  FullScreen -- Display
  yellow -- Color
  10 -- FPS
  "Vebjorn og Nikolai" -- initial world
  picture -- world transformation function
  inputEvent -- (Event -> World -> IO world)
  stepWorld -- (Float -> world -> IO world)

picture :: World -> IO Picture
picture t = return
  $ Translate (-170) (-20) -- shift the text to the middle of the window
  $ Scale 0.5 0.5 -- display it half the original size
  $ Text t -- text to display

inputEvent :: Event -> World -> IO World
inputEvent (EventKey (SpecialKey KeySpace) Down modifiers (x,y)) world = return $ " " ++ world
inputEvent (EventKey (Char 't') Down modifiers (x,y)) world = return "TROLL"
inputEvent _ world = return world

stepWorld :: Float -> World -> IO World
stepWorld step world = return world