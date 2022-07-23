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

data ActionLogParams = 
  AttackLogParams {
    attackLogged :: String,
    attackFailed :: Bool,
    effectiveness :: Float,
    isCrit :: Bool} |
  ChangeLogParams {
    previousPokemon :: String } |
  StatusLogParams {
    statusLog :: PokemonStatus
  } deriving (Eq, Show)

data ActionLog = ActionLog {
    playerInvolved :: Player, 
    pokemonName :: String,
    logParams :: ActionLogParams
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

data MovType = Buff | Dmg | Status deriving (Eq, Show)

data MovParams =
  DmgMov {
    power :: Int
  , dmgType :: DmgType } |
  BuffMov {
    multiplier :: Float
  , upgradedStats :: PokemonStats } |
  StatusMov {
    statusType :: PokemonStatus} 
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
                      ,  currentStatus :: Maybe PokemonStatus
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

data Action  = Movement Int | Change Int 

data State = InitialScreen | Running | ActionLogging | GameOver Player deriving (Eq, Show)

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
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Hoja afilada", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  }
  
  ,Pokemon {
    name = "Charmander"
  , pokedexNumber = 4
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Lapras"
  , pokedexNumber = 14
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 130, currentPs = 130, attack = 35, defense = 40, spAttack = 60, spDefense = 40, speed = 25, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Surf", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Paz Mental", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}]
  },
  Pokemon {
    name = "Venusaur"
  , pokedexNumber = 3
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 180, currentPs = 180, attack = 25, defense = 30, spAttack = 50, spDefense = 30, speed = 15, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Desarrollar", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 5, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Snorlax"
  , pokedexNumber = 15
  , stats = PokemonStatistics {pokemonType = [Normal], maxPs = 180, currentPs = 180, attack = 45, defense = 50, spAttack = 10, spDefense = 50, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Hiperrayo", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 100, dmgType = Physic}}
  , PokemonMov {attackName = "Panzazo", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 65, dmgType = Physic}}
  , PokemonMov {attackName = "Paz Mental", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}]
  },
  Pokemon {
    name = "Charizard"
  , pokedexNumber = 6
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 85, defense = 20, spAttack = 50, spDefense = 20, speed = 25, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 75, dmgType = Physic}}
  , PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  }]

generatePokemonTeamGary :: PokemonTeam
generatePokemonTeamGary = S.fromList [
  Pokemon {
    name = "Squirtle"
  , pokedexNumber = 7
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Burbuja", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Hidrochorro", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Physic}}]
  },
  Pokemon {
    name = "Arcanine"
  , pokedexNumber = 10
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 45, defense = 15, spAttack = 25, spDefense = 15, speed = 40, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Bola Fuego", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 35, dmgType = Physic}}
  , PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 55, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Victreebel"
  , pokedexNumber = 11
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 110, currentPs = 110, attack = 45, defense = 20, spAttack = 30, spDefense = 20, speed = 15, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Latigo cepa", movType = Dmg, movsLeft = 5, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 45, dmgType = Physic}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Gyarados"
  , pokedexNumber = 13
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 130, currentPs = 130, attack = 65, defense = 25, spAttack = 30, spDefense = 25, speed = 20, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Precision", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 4.0, upgradedStats = [Crit]}}]
  },
  Pokemon {
    name = "Exeggutor"
  , pokedexNumber = 12
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 140, currentPs = 140, attack = 25, defense = 20, spAttack = 45, spDefense = 20, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Desarrollar", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 5, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Blastoise"
  , pokedexNumber = 9
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 140, currentPs = 140, attack = 65, defense = 35, spAttack = 50, spDefense =25, speed = 40, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Surf", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Precision", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 4.0, upgradedStats = [Crit]}}]
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