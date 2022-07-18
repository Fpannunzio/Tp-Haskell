module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import System.Random

import Game
import Logic
import Rendering
import Types

window = InWindow "Pokemon-Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255


main :: IO ()
main = do
    charmanderImg <- loadBMP "assets/charmander.bmp"
    squirtleImg <- loadBMP "assets/squirtle.bmp"
    seed <- randomIO :: IO Int
    play window backgroundColor 30 (initialGame seed) (gameAsPicture [charmanderImg, squirtleImg]) transformGame (const id)
