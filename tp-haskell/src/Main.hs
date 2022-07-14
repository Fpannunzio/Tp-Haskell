module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Game
import Logic
import Rendering
import Types

window = InWindow "Pokemon-Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

main :: IO ()
main = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)
