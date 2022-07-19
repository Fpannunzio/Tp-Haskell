module Types where
import Data.Array
import qualified Data.Sequence as S

data PokemonType = Normal | Fuego | Agua | Hierba deriving (Eq, Show)

type PokemonAttributes = [PokemonType]
data PokemonStatistics = PokemonStatistics
  { pokemonType :: PokemonAttributes,
    maxPs :: Int ,
    currentPs :: Int,
    attack :: Int,
    defense :: Int,
    spAttack :: Int,
    spDefense :: Int,
    speed :: Int,
    crit :: Float
  } deriving (Eq, Show)

data PokemonStat = Attack | Defense | SpecialAttack | SpecialDefense | Speed | Crit deriving (Eq, Show)

type PokemonStats = [PokemonStat]

boostStat :: PokemonStat -> Float -> PokemonStatistics -> PokemonStatistics
boostStat Attack mult stats = stats { attack = multiplyAndFloor (attack stats)  mult }
boostStat Defense mult stats = stats{ defense = multiplyAndFloor (defense stats) mult }
boostStat SpecialAttack mult stats = stats { spAttack = multiplyAndFloor (spAttack stats) mult }
boostStat SpecialDefense mult stats = stats { spDefense = multiplyAndFloor (spDefense stats) mult }
boostStat Speed mult stats = stats { speed = multiplyAndFloor (speed stats) mult }
boostStat Crit mult stats = stats { crit = crit stats * mult }

data PokemonStatus = Poisoned | Paralized | Burned deriving (Eq, Show)

poisonedPorc :: Float
poisonedPorc = 0.15

burnedPorc :: Float
burnedPorc = 0.07

paralizedChance :: Float
paralizedChance = 0.5

data DmgType = Physic | Special deriving (Eq, Show)

getAttackingStat :: DmgType -> PokemonStatistics -> Int
getAttackingStat Physic pokStats = attack pokStats
getAttackingStat Special pokStats = spAttack pokStats

getDefensiveStat :: DmgType -> PokemonStatistics -> Int
getDefensiveStat Physic pokStats = defense pokStats
getDefensiveStat Special pokStats = spDefense pokStats

data MovType = Buff | Dmg | Status | Change deriving (Eq, Show)

data MovParams =
  DmgMov {
    power :: Int
  , dmgType :: DmgType } |
  BuffMov {
    multiplier :: Float
  , upgradedStats :: PokemonStats } |
  StatusMov {
    statusType :: PokemonStatus} |
  ChangeMov
 deriving (Eq, Show)

data PokemonMov = PokemonMov {
     attackName :: String
  ,  movType :: MovType
  ,  movsLeft :: Int
  ,  accuracy :: Float
  ,  pokType :: PokemonType
  ,  movParams :: MovParams
} deriving (Eq, Show)



type PokemonMovs = S.Seq PokemonMov

type Position = (Float, Float)
data Pokemon = Pokemon { name :: String
                      ,  pokedexNumber :: Int
                      ,  stats :: PokemonStatistics
                      ,  status :: Maybe PokemonStatus
                      ,  movs :: PokemonMovs
} deriving (Eq, Show)

type PokemonTeam = S.Seq Pokemon

invalidPokemon :: Int
invalidPokemon = 6

data Player = Ash | Gary deriving (Eq, Show)

otherPlayer :: Player -> Player
otherPlayer Ash = Gary
otherPlayer Gary = Ash

type Cell = String

data State = Running | GameOver Player deriving (Eq, Show)

typeTable :: PokemonType -> PokemonType -> Float
typeTable Fuego Agua = 0.5
typeTable Fuego Hierba = 2.0
typeTable Agua Hierba = 0.5
typeTable Agua Fuego = 2.0
typeTable Hierba Fuego = 0.5
typeTable Hierba Agua = 2.0
typeTable _ _ = 1.0


intToFloat :: Int -> Float
intToFloat number = fromIntegral number :: Float

multiplyAndFloor :: Int -> Float -> Int
multiplyAndFloor number mult = floor (intToFloat number * mult)

generatePokemonTeamAsh :: PokemonTeam
generatePokemonTeamAsh = S.fromList [
  Pokemon {
    name = "Bulbasaur"
  , pokedexNumber = 1
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  }
  ,Pokemon {
    name = "Ivysaur"
  , pokedexNumber = 2
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Venusaur"
  , pokedexNumber = 3
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  }
  ,Pokemon {
    name = "Charmander"
  , pokedexNumber = 4
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Charmilion"
  , pokedexNumber = 5
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Charizard"
  , pokedexNumber = 6
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  }]

generatePokemonTeamGary :: PokemonTeam
generatePokemonTeamGary = S.fromList [
  Pokemon {
    name = "Squirtle"
  , pokedexNumber = 7
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Burbuja", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Burned}}]
  }
  ,Pokemon {
    name = "Wartortle"
  , pokedexNumber = 8
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Burbuja", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Blastoise"
  , pokedexNumber = 9
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Burbuja", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Burned}}]
  }
  ,Pokemon {
    name = "Charmander"
  , pokedexNumber = 4
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Charmilion"
  , pokedexNumber = 5
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Charizard"
  , pokedexNumber = 6
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  }]
-- generatePokemonTeamP :: Int -> PokemonTeam
-- generatePokemonTeamP 0 = []
-- generatePokemonTeamP n =
  -- Pokemon {
  --   name = "Charmander"
  -- , pokedexNumber = 1
  -- , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  -- , status = Nothing
  -- , movs = fromList [
  --   PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  -- , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  -- , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  -- , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  -- } : generatePokemonTeamP(n-1)

-- generatePokemonTeamC :: Int -> PokemonTeam
-- generatePokemonTeamC 0 = []
-- generatePokemonTeamC n =
--   Pokemon {
--     name = "Squirtle"
--   , pokedexNumber = 2
--   , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.005}
--   , status = Nothing
--   , movs = fromList [
--     PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
--   , PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
--   , PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
--   , PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}]
--   }: generatePokemonTeamC(n-1)