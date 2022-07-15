module Game where
import Types
    ( Board,
      PokemonTeam,
      generatePokemonTeamP,
      generatePokemonTeamC,
      State(Running),
      Player(Ash) )
import Data.Array

data Game = Game { gameBoard :: Board
                 , firstPlayer :: Player
                 , ashTeam :: PokemonTeam
                 , garyTeam :: PokemonTeam
                 , gameState :: State
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

initialGame :: Game
initialGame = Game { gameBoard = array indexRange $ zip (range indexRange) (repeat "Ataque") --Aca voy a tener que escribir los nombres de los ataques (Como hacer esto a partir de los pokemones)
                   , firstPlayer = Ash
                   , ashTeam = generatePokemonTeamP 1
                   , garyTeam = generatePokemonTeamC 1
                   , gameState = Running
                   }
    where indexRange = ((0, 0), (n - 1, n - 1))


