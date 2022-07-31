module Colors where

import Graphics.Gloss
import Types

boardGridColor :: Color
boardGridColor = makeColorI 255 255 255 255

textColor :: Color
textColor = makeColorI 255 255 0 255

lifeColor :: Color
lifeColor = makeColorI 0 255 0 255

playerColor :: Player -> Color
playerColor Ash = makeColorI 255 50 50 255
playerColor Gary = makeColorI 50 100 255 255

pokemonStatusColor :: PokemonStatus -> Color
pokemonStatusColor Poisoned = makeColorI 153 0 153 255
pokemonStatusColor Paralized = makeColorI 255 255 0 255
pokemonStatusColor Burned = makeColorI 255 0 0 255

pokemonTypeColor :: PokemonType -> Color
pokemonTypeColor Normal = greyN 0.5
pokemonTypeColor Fuego = makeColorI 255 0 0 255
pokemonTypeColor Agua = makeColorI 0 0 255 255
pokemonTypeColor Hierba = makeColorI 0 255 0 255
pokemonTypeColor Electrico = makeColorI 255 255 0 255
pokemonTypeColor Tierra = makeColorI 153 106 20 255
pokemonTypeColor Roca = makeColorI 145 141 129 255
pokemonTypeColor Volador = makeColorI 199 255 240 255
