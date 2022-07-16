module Types where
import Data.Array
import Data.Sequence ( fromList, Seq )

data PokemonType = Normal | Fuego | Agua | Hierba deriving (Eq, Show)

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
data PokemonAttack = PokemonAttack { 
     attackName :: String
  ,  base :: Int
  ,  pokType :: PokemonType
  ,  attackType :: AttackType
  ,  movsLeft :: Int
} deriving (Eq, Show)

type PokemonMovs = Seq PokemonAttack

type Position = (Float, Float)
data Pokemon = Pokemon { name :: String
                      ,  stats :: PokemonStatistics
                      ,  movs :: PokemonMovs
} deriving (Eq, Show)

type PokemonTeam = [Pokemon]

data Player = Ash | Gary deriving (Eq, Show)

type Cell = String

data State = Running | GameOver Player deriving (Eq, Show)

intToFloat :: Int -> Float
intToFloat number = fromIntegral number :: Float

generatePokemonTeamP :: Int -> PokemonTeam
generatePokemonTeamP 0 = []
generatePokemonTeamP n =
  Pokemon {
    name = "Charmander"
  , stats = PokemonStatistics {pokemonType = Fuego, maxPs = 250, currentPs = 250, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , movs = fromList [
    PokemonAttack {attackName = "Placaje", base = 25, pokType = Normal, attackType = Physic, movsLeft = 10}
  , PokemonAttack {attackName = "Ascuas", base = 20, pokType = Fuego, attackType = Special, movsLeft = 10}
  , PokemonAttack {attackName = "Burbuja", base = 35, pokType = Agua, attackType = Physic, movsLeft = 5}
  , PokemonAttack {attackName = "Latigo", base = 15, pokType = Hierba, attackType = Physic, movsLeft = 10}]
  } : generatePokemonTeamP(n-1)

generatePokemonTeamC :: Int -> PokemonTeam
generatePokemonTeamC 0 = []
generatePokemonTeamC n =
  Pokemon {
    name = "Squirtle"
  , stats = PokemonStatistics {pokemonType = Agua, maxPs = 250, currentPs = 250, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , movs = fromList [
    PokemonAttack {attackName = "Placaje", base = 25, pokType = Normal, attackType = Physic, movsLeft = 10}
  , PokemonAttack {attackName = "Burbuja", base = 20, pokType = Agua, attackType = Special, movsLeft = 10}
  , PokemonAttack {attackName = "Mordida", base = 35, pokType = Normal, attackType = Physic, movsLeft = 5}
  , PokemonAttack {attackName = "Golpe", base = 15, pokType = Normal, attackType = Physic, movsLeft = 10}]
  }: generatePokemonTeamC(n-1)