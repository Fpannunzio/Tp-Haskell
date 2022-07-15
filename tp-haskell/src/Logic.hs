module Logic where

import Data.Array
import Data.Foldable ( asum )

import Graphics.Gloss.Interface.Pure.Game
import Game
import Types


toPositive :: Int -> Int
toPositive x
  | x > 0 = x
  | otherwise = 0

attackFormula :: Int -> Int -> Int -> Int
attackFormula attack defense attackDmg = toPositive (floor ((intToFloat(attackDmg * attack) / intToFloat defense) / 5.0))

processPhysicAttack :: PokemonStatistics -> PokemonStatistics -> PokemonAttack -> PokemonStatistics
processPhysicAttack attackingPokemonStats defensivePokemonStats pokemonAttack =
  let
    bpa = base pokemonAttack
    pokemonAD= attack attackingPokemonStats
    -- critChance = crit attackingPokemonStats
    defensivePS = currentPs defensivePokemonStats
    pokemonDefense = defense defensivePokemonStats
  in
    defensivePokemonStats {
      currentPs = toPositive (defensivePS - attackFormula pokemonAD pokemonDefense bpa)
    }

processSpecialAttack :: PokemonStatistics -> PokemonStatistics -> PokemonAttack -> PokemonStatistics
processSpecialAttack attackingPokemonStats defensivePokemonStats pokemonAttack =
  let
    bpa = base pokemonAttack
    pokemonAP = spAttack attackingPokemonStats
    -- critChance = crit attackingPokemonStats
    defensivePS = currentPs defensivePokemonStats
    pokemonSpDefense = spDefense defensivePokemonStats
  in
    defensivePokemonStats {
      currentPs = toPositive (defensivePS - attackFormula pokemonAP pokemonSpDefense bpa)
    }

processAttack :: Pokemon -> Pokemon -> PokemonAttack -> Pokemon
processAttack ap dp pa =
  let
    apStats = stats ap
    dpStats = stats dp
  in
    case attackType pa of
      Physic ->  dp { stats = processPhysicAttack apStats dpStats pa }
      Special ->  dp { stats = processSpecialAttack apStats dpStats pa }

-- Actualizo a los dos equipos
fight :: Pokemon -> Pokemon -> PokemonAttack -> PokemonAttack -> Game -> Game
fight ap gp apAttack gpAttack game =
  game {
    ashTeam = processAttack ap gp apAttack : tail (ashTeam game),
    garyTeam = processAttack gp ap gpAttack : tail (garyTeam game)
}

-- Si el segundo equipo es vacio siempre gana el primer jugador
-- Si el primero era vacio y no sali por el primer caso es que el segundo no lo es, gana el segundo jugador
-- Sino sigue igual
checkPlayerWon :: PokemonTeam -> PokemonTeam -> Player -> Player -> Game -> Game
checkPlayerWon _ [] firstPlayer _ game = game { gameState = GameOver firstPlayer}
checkPlayerWon [] _ _ secondPlayer game = game { gameState = GameOver secondPlayer}
checkPlayerWon _ _ _ _ game = game

-- Fijarse si alguna de las dos colecciones es vacias y en caso de serlo poner gameState en gameOver
checkStillFighting :: Game -> Game
checkStillFighting game =
    let
      at = ashTeam game
      gt = garyTeam game
    in
    case firstPlayer game of
      Ash -> checkPlayerWon at gt Ash Gary game
      Gary -> checkPlayerWon gt at Gary Ash game


checkPokemonPs :: Pokemon -> Bool
checkPokemonPs pokemon = currentPs (stats pokemon) == 0x0

swapAshPokemon :: Game -> Game
swapAshPokemon game
    | checkPokemonPs (head (ashTeam game)) = game { ashTeam = tail (ashTeam game) }
    | otherwise = game

swapGaryPokemon :: Game -> Game
swapGaryPokemon game
    | checkPokemonPs (head (garyTeam game)) = game { garyTeam = tail (garyTeam game) }
    | otherwise = game

-- Chequear que pokemones estan vivos y swappearlos funcion que pida game y devuelva game
swapDefetedPokemon :: Game -> Game
swapDefetedPokemon game
 = swapAshPokemon $ swapGaryPokemon game
    where
      ashCurrentPokemon = head (ashTeam game)
      garyCurrentPokemon = head (garyTeam game)
      ashRemaingPokemons = tail (ashTeam game)
      garyRemaingPokemons = tail (garyTeam game)

checkFirstAttack :: Pokemon -> Pokemon -> Game -> Game
checkFirstAttack ashPokemon garyPokemon game
    | ashSpeed > garySpeed = game { firstPlayer = Ash }
    | otherwise = game { firstPlayer = Gary }
    where
      ashSpeed = speed (stats ashPokemon)
      garySpeed = speed (stats garyPokemon)

fightPokemon :: Game -> Int -> Game
fightPokemon game attackNumber =
    let
      ashPokemon = head (ashTeam game)
      garyPokemon = head (garyTeam game)
      apAttack = head (movs ashPokemon) --TODO cambiar al numero del ataque
      gpAttack = head (movs ashPokemon)
    in
      checkStillFighting $
      swapDefetedPokemon $
      fight ashPokemon garyPokemon apAttack gpAttack $
      checkFirstAttack ashPokemon garyPokemon game

isCoordCorrect = inRange (0, 3)

cellCordToAttack :: (Int, Int) -> Int
cellCordToAttack cellCord =
  case cellCord of
    (0, 0) -> 0
    (0, 1) -> 1
    (1, 0) -> 2
    (1, 1) -> 3

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect attackNumber = fightPokemon game attackNumber
    | otherwise = game
    where attackNumber = cellCordToAttack cellCoord


mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      -- Running -> playerTurn game $ mousePosAsCellCoord mousePos
      Running -> playerTurn game (0,0)
      Fighting -> initialGame
      GameOver _ -> initialGame
transformGame _ game = game
