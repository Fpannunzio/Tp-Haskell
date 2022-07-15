module Rendering where

import Data.Array
import Graphics.Gloss

import Game
import Types

boardGridColor :: Color
boardGridColor = makeColorI 255 255 255 255

nameHeight :: Float
nameHeight = intToFloat screenHeight * 0.9 :: Float

lifeColor :: Color
lifeColor = makeColorI 0 255 0 255

lifeBarMaxLength :: Float
lifeBarMaxLength = intToFloat screenWidth * 0.15

lifeHeight :: Float
lifeHeight = intToFloat screenHeight * 0.8 :: Float

ashColor :: Color
ashColor = makeColorI 255 50 50 255

garyColor :: Color
garyColor = makeColorI 50 100 255 255

outcomeColor :: Player -> Color
outcomeColor Ash = ashColor
outcomeColor Gary = garyColor

defaultTextScale :: (Float, Float)
defaultTextScale = (0.3, 0.3)

ashPosition :: Float
ashPosition = intToFloat screenWidth * 0.05

garyPosition :: Float
garyPosition = intToFloat screenWidth * 0.6

-- Deberia mostrar, las dos fotos de los pokemones, su nombre, su vida y los ataques para elegir
gameRunningPicture :: Pokemon -> Pokemon -> Picture
gameRunningPicture ashPokemon garyPokemon =
    pictures [
              renderPokemon Gary garyPokemon
            , renderPokemon Ash ashPokemon
            , pokemonAttacksBoard ashPokemon
            , color boardGridColor boardGrid
             ]

renderPokemonName :: Float -> Color -> String -> Picture
renderPokemonName position currentColor name =
    translate position nameHeight
    $ uncurry scale defaultTextScale
    $ color currentColor
    $ text name

calculateLifeBarLength :: Float -> Int -> Int -> Float
calculateLifeBarLength initPosition currentPs maxPs = initPosition + (lifeBarMaxLength *  intToFloat currentPs / intToFloat maxPs)

lifeString :: Int -> Int -> String
lifeString currentPs maxPs = show currentPs ++ "/" ++ show maxPs

calculateLifePointsPosition :: Float -> Float
calculateLifePointsPosition initPosition = initPosition + lifeBarMaxLength * 1.1

renderPokemonLife :: Float -> Int -> Int -> Picture
renderPokemonLife position currentPs maxPs =
  pictures [
  color lifeColor
  $ line [(position, lifeHeight), (calculateLifeBarLength position currentPs maxPs, lifeHeight)]
  ,
  translate (calculateLifePointsPosition position) lifeHeight
  $ uncurry scale defaultTextScale
  $ color lifeColor
  $ text (lifeString currentPs maxPs)
  ]

renderPokemon :: Player -> Pokemon -> Picture
renderPokemon Ash pokemon = pictures [
      renderPokemonName ashPosition ashColor (name pokemon)
    , renderPokemonLife ashPosition (maxPs (stats pokemon)) (currentPs (stats pokemon)) ]
renderPokemon Gary pokemon = pictures [
      renderPokemonName garyPosition garyColor (name pokemon)
    , renderPokemonLife garyPosition (maxPs (stats pokemon)) (currentPs (stats pokemon))]

boardGrid :: Picture
boardGrid =
    pictures
     [ line [ (fromIntegral screenWidth / 2, 0.0)
            , (fromIntegral screenWidth / 2, fromIntegral screenHeight/2)
            ]
    , line [ (0.0, 0.0)
            , (fromIntegral screenWidth, 0.0)
            ]
    , line [ (0.0, cellHeight/2)
            , (fromIntegral screenWidth, cellHeight/2)
            ]
    , line [ (0.0, cellHeight)
            , (fromIntegral screenWidth, cellHeight)
            ]
    ]

-- Deberia mostrar, las dos fotos de los pokemones, su nombre y su vida
gameFightingPicture :: Pokemon -> Pokemon -> Picture
gameFightingPicture ashPokemon garyPokemon =
    pictures [ color ashColor $ text (name ashPokemon)
             , color garyColor $ text (name garyPokemon)
             , pokemonAttacksBoard ashPokemon
             ]
-- Translate te pone el pixel abajo
-- IMPORTANTE primero escalar y despues traducir
pokemonAttacksBoard :: Pokemon -> Picture
pokemonAttacksBoard ashPokemon =  pictures (map (translate (cellWidth * 0.25) (cellHeight * 0.7) . uncurry scale defaultTextScale . color ashColor . Text . attackName) (movs ashPokemon))

boardAsGameOverPicture :: Player -> Picture
boardAsGameOverPicture winner = case winner of
                        Ash -> color (outcomeColor Ash) (Text "Ash wins")
                        Gary -> color (outcomeColor Gary) (Text "Gary wins")

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> gameRunningPicture (head (ashTeam game)) (head (garyTeam game))
                    Fighting -> gameFightingPicture (head (ashTeam game)) (head (garyTeam game))
                    GameOver winner -> boardAsGameOverPicture winner
