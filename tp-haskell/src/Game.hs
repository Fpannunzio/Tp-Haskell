module Game where
import Types
import Random
import Data.Sequence ( index )
data Game = Game { firstPlayer :: Player
                 , currentPlayer :: Player
                 , ashTeam :: PokemonTeam
                 , garyTeam :: PokemonTeam
                 , ashPokemon :: Int
                 , garyPokemon :: Int
                 , gameState :: State
                 , actions :: [ActionLog]
                 , seeds :: [Int]
                 } deriving (Eq, Show)

n :: Int
n = 2

screenWidth :: Int
screenWidth = 1120

screenHeight :: Int
screenHeight = 840

cellWidth :: Float
cellWidth = fromIntegral screenWidth / fromIntegral n

cellHeight :: Float
cellHeight = fromIntegral screenHeight / (fromIntegral n * 2.0)

getSeeds :: Int -> Game -> [Int]
getSeeds amount game = take amount (seeds game)

getSeed :: Game -> (Int, Game)
getSeed game = (head (seeds game), game {seeds = tail (seeds game)})
 
getPlayerPokemonIndex :: Player -> Game -> Int
getPlayerPokemonIndex Ash game = ashPokemon game
getPlayerPokemonIndex Gary game = garyPokemon game

getPlayerTeam :: Player -> Game -> PokemonTeam
getPlayerTeam Ash game = ashTeam game
getPlayerTeam Gary game = garyTeam game

getPlayerPokemon :: Player -> Game -> Pokemon
getPlayerPokemon player game = getPlayerTeam player game `index` getPlayerPokemonIndex player game

updatePlayerTeam :: Player -> PokemonTeam -> Game -> Game
updatePlayerTeam Ash newTeam game = game {ashTeam = newTeam}
updatePlayerTeam Gary newTeam game = game {garyTeam = newTeam}

updatePlayerCurrentPokemon :: Player -> Int -> Game -> Game
updatePlayerCurrentPokemon Ash newPokemon game = game {ashPokemon = newPokemon}
updatePlayerCurrentPokemon Gary newPokemon game = game {garyPokemon = newPokemon}

appendLog :: ActionLog -> Game -> Game
appendLog log game =
  let 
    actionsLogged = actions game
  in
  game {
      actions = log : actionsLogged
    }

preppendLog :: ActionLog -> Game -> Game
preppendLog log game =
  let 
    actionsLogged = actions game
  in
  game {
      actions = actionsLogged ++ [log]
    }

initialGame :: Int -> Game
initialGame seed = Game { 
                    firstPlayer = Ash
                   , currentPlayer = Ash
                   , ashTeam = generatePokemonTeamAsh
                   , garyTeam = generatePokemonTeamGary
                   , ashPokemon = 0
                   , garyPokemon = 0
                   , gameState = InitialScreen
                   , actions = []
                   , seeds = numbers
                   }
    where numbers = randomList seed


