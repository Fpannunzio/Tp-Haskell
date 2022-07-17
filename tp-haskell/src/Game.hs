module Game where
import Types
import Random
data Game = Game { firstPlayer :: Player
                 , ashTeam :: PokemonTeam
                 , garyTeam :: PokemonTeam
                 , gameState :: State
                 , seeds :: [Int]
                 } deriving (Eq, Show)

n :: Int
n = 2

screenWidth :: Int
screenWidth = 640

screenHeight :: Int
screenHeight = 480

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / (fromIntegral n * 2.0)


getSeeds :: Int -> Game -> [Int]
getSeeds amount game = take amount (seeds game)

removeSeed :: Game -> Game
removeSeed game = 
  game {
    seeds = tail (seeds game)
  }

initialGame :: Int -> Game
initialGame seed = Game { firstPlayer = Ash
                   , ashTeam = generatePokemonTeamP 2
                   , garyTeam = generatePokemonTeamC 2
                   , gameState = Running
                   , seeds = numbers
                   }
    where numbers = randomList seed


