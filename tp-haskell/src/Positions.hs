module Positions where

import Game
import Types
import qualified Data.Sequence as S

type Position = (Float, Float)

adjustedScreenPosition :: Position
adjustedScreenPosition = (fromIntegral screenWidth * (-0.5), fromIntegral screenHeight * (-0.5))

middleOfScreen :: Float
middleOfScreen = fromIntegral screenHeight * 0.5 :: Float

gameLogoPosition :: Position
gameLogoPosition = (fromIntegral screenWidth * 0.5 :: Float, fromIntegral screenHeight * 0.75 :: Float)

firstInstructionsPosition :: Position
firstInstructionsPosition = (fromIntegral screenWidth * 0.10 :: Float, fromIntegral screenHeight * 0.35 :: Float)

secondInstructionsPosition :: Position
secondInstructionsPosition = (fromIntegral screenWidth * 0.10 :: Float, fromIntegral screenHeight * 0.25 :: Float)

thirdInstructionsPosition :: Position
thirdInstructionsPosition = (fromIntegral screenWidth * 0.10 :: Float, fromIntegral screenHeight * 0.10 :: Float)

lifeBarMaxLength :: Float
lifeBarMaxLength = fromIntegral screenWidth * 0.15

nameHeight :: Float
nameHeight = fromIntegral screenHeight * 0.9 :: Float

lifeHeight :: Float
lifeHeight = fromIntegral screenHeight * 0.85 :: Float

spriteHeight :: Float
spriteHeight = fromIntegral screenHeight * 0.65 :: Float

pokeballHeight :: Float
pokeballHeight = fromIntegral screenHeight * 0.80 :: Float

logHeight :: Float
logHeight = middleOfScreen - logPosition

playerPosition :: Player -> Float
playerPosition Ash = fromIntegral screenWidth * 0.05
playerPosition Gary = fromIntegral screenWidth * 0.6

pokeballPosition :: Float
pokeballPosition = fromIntegral screenWidth * 0.03

logPosition :: Float
logPosition = fromIntegral screenHeight * 0.1

spritePosition :: Float
spritePosition = fromIntegral screenWidth * 0.15

statusPosition :: Float
statusPosition = fromIntegral screenWidth * 0.2

winnerPosition :: Position
winnerPosition = (0.20, fromIntegral screenHeight * 0.45)

attackPositions :: S.Seq Position
attackPositions = S.fromList [(cellWidth * 0.1, cellHeight * 1.3), (cellWidth * 1.1, cellHeight * 1.3), (cellWidth * 0.1, cellHeight * 0.3), (cellWidth * 1.1, cellHeight * 0.3)]
