module Logic where

import Data.Sequence

import Graphics.Gloss.Interface.Pure.Game
import Game
import Types
import Random

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

substractedAttack :: PokemonAttack -> PokemonAttack
substractedAttack attack =
  let
    currentMovs = movsLeft attack
  in
    attack {
      movsLeft = currentMovs - 1
    }

substractAttackMovAux :: Pokemon -> Int -> Pokemon
substractAttackMovAux pok attackIndex =
  let
    allMovs = movs pok
    attack =  Data.Sequence.lookup attackIndex allMovs -- equivalente a hacer allMovs !! attackIndex
  in
    case attack of
      Nothing -> pok
      Just att -> pok {
        movs = Data.Sequence.update attackIndex (substractedAttack att) allMovs
    }

substractAttackMov :: Int -> Int -> Game -> Game
substractAttackMov apAttack gpAttack game =
  let
    ashPokemon = head (ashTeam game)
    garyPokemon = head (garyTeam game)
  in
    game {
      ashTeam =  substractAttackMovAux ashPokemon apAttack : tail (ashTeam game), --Se lee, el pokemon de Gary ataca al pokemon de ash con el ataque gpAttack
      garyTeam = substractAttackMovAux garyPokemon gpAttack : tail (garyTeam game)
    }


toPositive :: Int -> Int
toPositive x
  | x > 0 = x
  | otherwise = 0

calculateCrit :: Float -> Int -> Float
calculateCrit critChance seed = 
  let
    probability = randomProbability seed
  in
    if probability < critChance then 2.0
    else 1.0

calculateStab :: PokemonAttributes -> PokemonType -> Float
calculateStab attackingAtr attackType = if attackType `elem` attackingAtr then 1.5 else 1

calculateEffectiveness :: PokemonAttributes -> PokemonType -> Float
calculateEffectiveness defendingAtr attackType = product (map (typeTable attackType) defendingAtr)

attackFormula :: Int -> Float -> Int -> Int -> PokemonAttributes -> PokemonAttributes -> PokemonAttack -> Int -> Int
attackFormula attack critChance defense defensivePS attackingAtr defendingAtr pokemonAttack seed =
  let
    attackDmg = base pokemonAttack
    attackType = pokType pokemonAttack
    crit = calculateCrit critChance seed
    attackStab = calculateStab attackingAtr attackType
    efectiveness = calculateEffectiveness defendingAtr attackType
  in
  toPositive(defensivePS - toPositive (floor ((intToFloat(attackDmg * attack) * attackStab * efectiveness * crit / intToFloat defense) / 5.0)))

processPhysicAttack :: PokemonStatistics -> PokemonStatistics -> PokemonAttack -> Int -> PokemonStatistics
processPhysicAttack attackingPokemonStats defensivePokemonStats pokemonAttack seed =
  let
    attackingPokemonTypes = pokemonType attackingPokemonStats
    defendingPokemonTypes = pokemonType defensivePokemonStats
    critChance = crit attackingPokemonStats
    attackingAttack = attack attackingPokemonStats
    defensivePS = currentPs defensivePokemonStats
    defensiveDefense = defense defensivePokemonStats
  in
    defensivePokemonStats {
      currentPs = attackFormula attackingAttack critChance defensiveDefense defensivePS attackingPokemonTypes defendingPokemonTypes pokemonAttack seed
    }

processSpecialAttack :: PokemonStatistics -> PokemonStatistics -> PokemonAttack -> Int -> PokemonStatistics
processSpecialAttack attackingPokemonStats defensivePokemonStats pokemonAttack seed =
  let
    attackingPokemonTypes = pokemonType attackingPokemonStats
    defendingPokemonTypes = pokemonType defensivePokemonStats
    critChance = crit attackingPokemonStats
    attackingSpAttack = spAttack attackingPokemonStats
    defensivePS = currentPs defensivePokemonStats
    defensiveSpDefense = spDefense defensivePokemonStats
  in
    defensivePokemonStats {
      currentPs = attackFormula attackingSpAttack critChance defensiveSpDefense defensivePS attackingPokemonTypes defendingPokemonTypes pokemonAttack seed
    }

processAttack :: Pokemon -> Pokemon -> PokemonAttack -> Int -> Pokemon
processAttack attackingPokemon defendingPokemon pokemonAttack seed =
  let
    apStats = stats attackingPokemon
    dpStats = stats defendingPokemon
  in
    case attackType pokemonAttack of
      Physic ->  defendingPokemon { stats = processPhysicAttack apStats dpStats pokemonAttack seed }
      Special ->  defendingPokemon { stats = processSpecialAttack apStats dpStats pokemonAttack seed }

-- Actualizo a los dos equipos
fight :: Pokemon -> Pokemon -> PokemonAttack -> PokemonAttack -> Game -> Game
fight ashPokemon garyPokemon apAttack gpAttack game =
  let
    seeds = getSeeds 2 game
    firstSeed = head seeds
    secondSeed = seeds !! 1
  in
  -- Tengo que eliminar las dos seeds que ya use 
  removeSeed $
  removeSeed $
  game {
    ashTeam =  processAttack garyPokemon ashPokemon gpAttack firstSeed : tail (ashTeam game), --Se lee, el pokemon de Gary ataca al pokemon de ash con el ataque gpAttack
    garyTeam = processAttack ashPokemon garyPokemon apAttack secondSeed: tail (garyTeam game)
}

checkFirstAttack :: Pokemon -> Pokemon -> Game -> Game
checkFirstAttack ashPokemon garyPokemon game
    | ashSpeed > garySpeed = game { firstPlayer = Ash }
    | otherwise = game { firstPlayer = Gary }
    where
      ashSpeed = speed (stats ashPokemon)
      garySpeed = speed (stats garyPokemon)

checkMaybeGaryValue :: Maybe Int -> Int
checkMaybeGaryValue Nothing = 4
checkMaybeGaryValue (Just i) = i

checkMaybeAttack :: Maybe PokemonAttack -> PokemonAttack
checkMaybeAttack Nothing = PokemonAttack {attackName = "Placaje", base = 25, pokType = Normal, attackType = Physic, movsLeft = 10}
checkMaybeAttack (Just pa) = pa

garyAttackPick :: Pokemon -> Int -> Int
garyAttackPick pokemon seed =
  let
    pokemonMovs = movs pokemon
    nonEmptyAttacks = Data.Sequence.filter stillHasMoves pokemonMovs
    number = randomAttack seed (0, Data.Sequence.length nonEmptyAttacks - 1)
  in
    if Data.Sequence.null nonEmptyAttacks then 5
    else checkMaybeGaryValue (Data.Sequence.elemIndexL (checkMaybeAttack(Data.Sequence.lookup number nonEmptyAttacks)) pokemonMovs)

stillHasMoves :: PokemonAttack -> Bool
stillHasMoves pokemonAttack = movsLeft pokemonAttack > 0

fightPokemon :: Game -> Int -> Game
fightPokemon game attackNumber
    -- | not (isCoordCorrect garyAttackNumber) = game --TODO Sacar el pokemon de Gary
    | stillHasMoves apAttack =
      checkStillFighting
        $ swapDefetedPokemon
        $ substractAttackMov attackNumber garyAttackNumber
        $ fight ashPokemon garyPokemon apAttack gpAttack
        $ checkFirstAttack ashPokemon garyPokemon 
        $ removeSeed game
    | otherwise = game
    where
      ashPokemon = head (ashTeam game)
      garyPokemon = head (garyTeam game)
      apAttack = checkMaybeAttack (Data.Sequence.lookup attackNumber (movs ashPokemon))
      seed = head (getSeeds 1 game)
      garyAttackNumber = garyAttackPick garyPokemon seed
      gpAttack = checkMaybeAttack (Data.Sequence.lookup garyAttackNumber (movs garyPokemon))

isCoordCorrect :: Int -> Bool
isCoordCorrect x = x >= 0 && x <=3 -- inRange?

cellCordToAttack :: (Int, Int) -> Int
cellCordToAttack cellCord =
  case cellCord of
    (0, 1) -> 0
    (1, 1) -> 1
    (0, 0) -> 2
    (1, 0) -> 3
    _ -> 4

playerTurn :: Game -> (Int, Int) -> Game
playerTurn game cellCoord
    | isCoordCorrect attackNumber = fightPokemon game attackNumber
    | otherwise = game
    where attackNumber = cellCordToAttack cellCoord


mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = (floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth),
                              floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight))

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      -- Running -> playerTurn game $ mousePosAsCellCoord mousePos
      Running -> playerTurn game (mousePosAsCellCoord mousePos)
      GameOver _ -> initialGame (head (getSeeds 1 game))
transformGame _ game = game 
