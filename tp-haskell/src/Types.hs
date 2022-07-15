module Types where

import Data.Array

type Board = Array (Int, Int) String -- Quiero celdas que tengan los ataques

data PokemonType = Fuego | Agua | Hierba deriving (Eq, Show)

data PokemonStatistics = PokemonStatistics
  { pokemonType :: PokemonType,
    maxPs :: Int ,
    currentPs :: Int,
    attack :: Int,
    defense :: Int,
    spAttack :: Int,
    spDefense :: Int,
    speed :: Int,
    crit :: Double
  } deriving (Eq, Show)

data AttackType = Physic | Special deriving (Eq, Show)
data PokemonAttack = PokemonAttack { attackName :: String
                                  ,  base :: Int
                                  ,  attackType :: AttackType
} deriving (Eq, Show)

type PokemonMovs = [PokemonAttack]
data Pokemon = Pokemon { name :: String
                      ,  stats :: PokemonStatistics
                      ,  movs :: PokemonMovs
} deriving (Eq, Show)

type PokemonTeam = [Pokemon]

data Player = Ash | Gary deriving (Eq, Show)

type Cell = String

data State = Running | Fighting | GameOver Player deriving (Eq, Show)

intToFloat :: Int -> Float
intToFloat number = fromIntegral number :: Float

generatePokemonTeamP :: Int -> PokemonTeam
generatePokemonTeamP 0 = []
generatePokemonTeamP n =
  Pokemon {
    name = "Charmander"
  , stats = PokemonStatistics {pokemonType = Fuego, maxPs = 25, currentPs = 25, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , movs = [PokemonAttack {attackName = "Placaje", base = 25, attackType = Physic}]
  } : generatePokemonTeamP(n-1)

generatePokemonTeamC :: Int -> PokemonTeam
generatePokemonTeamC 0 = []
generatePokemonTeamC n =
  Pokemon {
    name = "Squirtle"
  , stats = PokemonStatistics {pokemonType = Agua, maxPs = 25, currentPs = 25, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , movs = [PokemonAttack {attackName = "Placaje", base = 25, attackType = Physic}]
  } : generatePokemonTeamC(n-1)