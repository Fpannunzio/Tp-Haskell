module Rendering where

import Data.Array
import Graphics.Gloss

import Game
import Types

boardGridColor :: Color
boardGridColor = makeColorI 255 255 255 255

ashColor :: Color
ashColor = makeColorI 255 50 50 255

garyColor :: Color
garyColor = makeColorI 50 100 255 255

outcomeColor :: Player -> Color
outcomeColor Ash = ashColor
outcomeColor Gary = garyColor

-- Deberia mostrar, las dos fotos de los pokemones, su nombre, su vida y los ataques para elegir
gameRunningPicture :: Pokemon -> Pokemon -> Picture
gameRunningPicture ashPokemon garyPokemon =
    pictures [ 
              -- color ashColor $ text (name ashPokemon)
            --  ,color garyColor $ text (name garyPokemon)
             pokemonAttacksBoard ashPokemon
             ]
-- Deberia mostrar, las dos fotos de los pokemones, su nombre y su vida
gameFightingPicture :: Pokemon -> Pokemon -> Picture
gameFightingPicture ashPokemon garyPokemon =
    pictures [ color ashColor $ text (name ashPokemon)
             , color garyColor $ text (name garyPokemon)
             , pokemonAttacksBoard ashPokemon
             ]

pokemonAttacksBoard :: Pokemon -> Picture
pokemonAttacksBoard ashPokemon =  pictures (map (Color ashColor . Text . attackName) (movs ashPokemon))

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
