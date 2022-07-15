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

defaultTextScale :: Position
defaultTextScale = (0.3, 0.3)

attackTextScale :: Position
attackTextScale = (0.4, 0.4)

ashPosition :: Float
ashPosition = intToFloat screenWidth * 0.05

garyPosition :: Float
garyPosition = intToFloat screenWidth * 0.6

winnerPosition :: Position
winnerPosition = (0.0, intToFloat screenHeight * 0.45)

attackPositions :: [Position]
attackPositions = [(cellWidth * 0.25, cellHeight * 0.7), (cellWidth * 1.25, cellHeight * 0.7), (cellWidth * 0.25, cellHeight * 0.2), (cellWidth * 1.25, cellHeight * 0.2)]

pokemonTypeColor :: PokemonType -> Color
pokemonTypeColor Normal = greyN 0.5
pokemonTypeColor Fuego = makeColorI 255 0 0 255
pokemonTypeColor Agua = makeColorI 0 0 255 255
pokemonTypeColor Hierba = makeColorI 0 255 0 255


-- Deberia mostrar, las dos fotos de los pokemones, su nombre, su vida y los ataques para elegir
gameRunningPicture :: Pokemon -> Pokemon -> Picture
gameRunningPicture ashPokemon garyPokemon =
    pictures [
              renderPokemon Gary garyPokemon
            , renderPokemon Ash ashPokemon
            , pokemonAttacksBoard (movs ashPokemon)
            , color boardGridColor boardGrid
             ]

renderPokemonName :: Float -> Color -> String -> Picture
renderPokemonName position currentColor name =
    translate position nameHeight
    $ uncurry scale defaultTextScale
    $ color currentColor
    $ text name

calculateLifeBarLength :: Float -> Int -> Int -> Float
calculateLifeBarLength initPosition currentPs maxPs = initPosition + lifeBarMaxLength * intToFloat maxPs / intToFloat currentPs

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

renderPokemonAttack :: PokemonAttack -> Position -> Picture
renderPokemonAttack pokemonAttack pos = uncurry translate pos (uncurry scale attackTextScale (color (pokemonTypeColor (pokType pokemonAttack))  (Text  (attackName pokemonAttack))))

-- Translate te pone el pixel abajo
-- IMPORTANTE primero escalar y despues traducir
pokemonAttacksBoard :: PokemonMovs -> Picture
pokemonAttacksBoard pokeMovs = pictures (zipWith renderPokemonAttack pokeMovs attackPositions)
-- pokemonAttacksBoard ashPokemon  =  pictures (map (uncurry translate  . uncurry scale defaultTextScale . color ashColor . Text . attackName) (movs ashPokemon))

boardAsGameOverPicture :: Player -> Picture
boardAsGameOverPicture winner = uncurry translate winnerPosition
                     $ color (outcomeColor winner)
                     $ text (show winner ++ " wins")

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> gameRunningPicture (head (ashTeam game)) (head (garyTeam game))
                    GameOver winner -> boardAsGameOverPicture winner
