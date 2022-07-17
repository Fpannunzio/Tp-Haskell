module Rendering where

import Data.Array
import Data.Foldable (toList)
import Data.Sequence ( fromList, Seq)

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

defaultTextScale :: Position
defaultTextScale = (0.3, 0.3)

attackTextScale :: Position
attackTextScale = (0.4, 0.4)

playerColor :: Player -> Color
playerColor Ash = makeColorI 255 50 50 255
playerColor Gary = makeColorI 50 100 255 255

playerPosition :: Player -> Float
playerPosition Ash = intToFloat screenWidth * 0.05
playerPosition Gary = intToFloat screenWidth * 0.6

statusPosition :: Float
statusPosition = intToFloat screenWidth * 0.25

winnerPosition :: Position
winnerPosition = (0.0, intToFloat screenHeight * 0.45)

attackPositions :: [Position]
attackPositions = [(cellWidth * 0.05, cellHeight * 1.2), (cellWidth * 1.05, cellHeight * 1.2), (cellWidth * 0.05, cellHeight * 0.2), (cellWidth * 1.05, cellHeight * 0.2)]

pokemonStatusColor :: PokemonStatus -> Color
pokemonStatusColor Poisoned = makeColorI 153 0 153 255
pokemonStatusColor Paralized = makeColorI 255 255 0 255
pokemonStatusColor Burned = makeColorI 255 0 0 255

statusText :: PokemonStatus -> String
statusText Poisoned = "ENV"
statusText Paralized = "PAR"
statusText Burned = "QUE"

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
calculateLifeBarLength initPosition currentPs maxPs = initPosition + lifeBarMaxLength * intToFloat currentPs / intToFloat maxPs

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

renderPokemonStatus :: Float -> Maybe PokemonStatus -> Picture
renderPokemonStatus position Nothing = Blank
renderPokemonStatus position (Just status) = 
  translate position nameHeight
  $ uncurry scale defaultTextScale
  $ color (pokemonStatusColor status) 
  $ text (statusText status)


renderPokemon :: Player -> Pokemon -> Picture
renderPokemon player pokemon = pictures [
      renderPokemonName (playerPosition player) (playerColor player) (name pokemon)
    , renderPokemonStatus (playerPosition player + statusPosition) (status pokemon) 
    , renderPokemonLife (playerPosition player) (currentPs (stats pokemon)) (maxPs (stats pokemon))]

boardGrid :: Picture
boardGrid =
    pictures
     [ line [ (cellWidth, 0.0)
            , (cellWidth, cellHeight * 2) -- Linea |
            ]
    , line [ (0.0, 0.0)
            , (cellWidth * 2, 0.0) -- Linea __
            ]
    , line [ (0.0, cellHeight)
            , (cellWidth * 2, cellHeight) -- Linea --
            ]
    , line [ (0.0, cellHeight * 2)
            , (cellWidth * 2, cellHeight * 2)
            ]
    ]

attackString :: Int -> String -> String
attackString movCount attName = show movCount ++ "-" ++ attName

renderPokemonAttack :: PokemonMov -> Position -> Picture
renderPokemonAttack pokemonAttack pos =
  let
    movCount = movsLeft pokemonAttack
    attName = attackName pokemonAttack
  in
  uncurry translate pos (uncurry scale attackTextScale (color (pokemonTypeColor (pokType pokemonAttack))  (Text (attackString movCount attName))))

-- Translate te pone el pixel abajo
-- IMPORTANTE primero escalar y despues traducir
pokemonAttacksBoard :: PokemonMovs -> Picture
pokemonAttacksBoard pokeMovs = pictures (zipWith renderPokemonAttack (toList pokeMovs) attackPositions)
-- pokemonAttacksBoard ashPokemon  =  pictures (map (uncurry translate  . uncurry scale defaultTextScale . color ashColor . Text . attackName) (movs ashPokemon))

boardAsGameOverPicture :: Player -> Picture
boardAsGameOverPicture winner = uncurry translate winnerPosition
                     $ color (playerColor winner)
                     $ text (show winner ++ " wins")

gameAsPicture :: Game -> Picture
gameAsPicture game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    Running -> gameRunningPicture (head (ashTeam game)) (head (garyTeam game))
                    GameOver winner -> boardAsGameOverPicture winner
