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
    isCrit :: Bool,
    effectiveness :: Float} |
  ChangeLogParams {
    defeated :: Bool,
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
    statusType :: PokemonStatus} deriving (Eq, Show)

data PokemonMov = PokemonMov {
     attackName :: String
  ,  movType :: MovType
  ,  movsLeft :: Int
  ,  accuracy :: Float
  ,  pokType :: PokemonType
  ,  movParams :: MovParams
} deriving (Eq, Show)

type PokemonMovPair = (Int, PokemonMov)

type PokemonMovs = S.Seq PokemonMov


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

data Action  = Movement Int | Change Int 

typeTable :: PokemonType -> PokemonType -> Float
typeTable Fuego Agua = 0.5
typeTable Fuego Hierba = 2.0
typeTable Agua Hierba = 0.5
typeTable Agua Fuego = 2.0
typeTable Hierba Fuego = 0.5
typeTable Hierba Agua = 2.0
typeTable _ _ = 1.0

multiplyAndFloor :: Int -> Float -> Int
multiplyAndFloor number mult = floor (fromIntegral number * mult)

