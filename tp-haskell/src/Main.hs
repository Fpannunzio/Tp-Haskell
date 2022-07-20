module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.Color
import System.Random

import Game
import Logic
import Rendering
import Types
import Data.Sequence ( fromList )

window = InWindow "Pokemon-Functional" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

loadImages :: [String] -> [IO Picture]
loadImages = map loadBMP

retrieveSprites :: Maybe [Picture] -> [Picture]
retrieveSprites Nothing = []
retrieveSprites (Just sprites) = sprites

main :: IO ()
main = do
    gameLogo <- loadBMP "assets/gameLogo.bmp"
    ashImagesFile <- readFile "assets/ashImagesFile"
    garyImagesFile <- readFile "assets/garyImagesFile"
    seed <- randomIO :: IO Int
    ashImages <- sequence $ loadImages $ lines ashImagesFile
    garyImages <- sequence $ loadImages $ lines garyImagesFile
    play window backgroundColor 30 (initialGame seed) (gameAsPicture gameLogo (fromList ashImages) (fromList garyImages)) transformGame (const id)
