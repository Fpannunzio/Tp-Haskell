module Logic where

import qualified Data.Sequence as S
import qualified Data.Maybe (fromMaybe)

import Graphics.Gloss.Interface.Pure.Game
import Game
import Types
import Random

-- Si el segundo equipo perdio gana el primer jugador siempre
-- Si el primero perdio y no sali por el primer caso es que el segundo no lo es, gana el segundo jugador
-- Sino sigue igual

checkPlayerWon :: Bool -> Bool -> Player -> Player -> Game -> Game
checkPlayerWon _ False firstPlayer _ game = game { gameState = GameOver firstPlayer}
checkPlayerWon False _ _ secondPlayer game = game { gameState = GameOver secondPlayer}
checkPlayerWon _ _ _ _ game = game

-- Fijarse si alguna de las dos colecciones es vacias y en caso de serlo poner gameState en gameOver
checkStillFighting :: Game -> Game
checkStillFighting game =
    let
      at = ashTeam game
      gt = garyTeam game
      ashStillFighting = getFirstAlive at < invalidPokemon
      garyStillFighting = getFirstAlive gt < invalidPokemon
    in
    case firstPlayer game of
      Ash -> checkPlayerWon ashStillFighting garyStillFighting Ash Gary game
      Gary -> checkPlayerWon garyStillFighting ashStillFighting Gary Ash game


checkDefeated :: Pokemon -> Bool
checkDefeated pokemon = currentPs (stats pokemon) == 0x0

getFirstAlive :: PokemonTeam -> Int
getFirstAlive team =
  let
    alivePokemons = S.filter (not . checkDefeated) team
  in
    if S.null alivePokemons then invalidPokemon
    else Data.Maybe.fromMaybe
      invalidPokemon (S.elemIndexL (alivePokemons `S.index` 0) team)


swapPlayerDefeatedPokemon ::Player -> Game -> Game
swapPlayerDefeatedPokemon player game =
  let
    pokemon = getPlayerPokemon player game
    pokemonTeam = getPlayerTeam player game
    firstAlivePokemon = getFirstAlive pokemonTeam
  in
    if checkDefeated pokemon && firstAlivePokemon < invalidPokemon then
      updatePlayerCurrentPokemon player firstAlivePokemon game
    else
      game

-- Chequear que pokemones estan vivos y swappearlos funcion que pida game y devuelva game
swapDefetedPokemon :: Game -> Game
swapDefetedPokemon game = swapPlayerDefeatedPokemon Gary $ swapPlayerDefeatedPokemon Ash game

statusFormula :: Int -> Int -> Float -> Int
statusFormula maxPs currentPs mult = toPositive (currentPs - multiplyAndFloor maxPs mult)

applyStatusCondition :: Pokemon -> Pokemon
applyStatusCondition currentPokemon =
  let
    pokemonStatus = status currentPokemon
    pokemonStats = stats currentPokemon
    currentPokemonPs = currentPs pokemonStats
    currentPokemonMaxPs = maxPs pokemonStats
  in
    case pokemonStatus of
      Just Poisoned -> currentPokemon {stats = pokemonStats {currentPs = statusFormula currentPokemonMaxPs currentPokemonPs poisonedPorc}}
      Just Burned -> currentPokemon {stats = pokemonStats {currentPs = statusFormula currentPokemonMaxPs currentPokemonPs burnedPorc}}
      _ -> currentPokemon

applyStatusEffectAux ::Player -> Game -> Game
applyStatusEffectAux player game =
  let
    pokemonIndex = getPlayerPokemonIndex player game
    pokemon = getPlayerPokemon player game
    team = getPlayerTeam player game
  in
    updatePlayerTeam player (S.update pokemonIndex (applyStatusCondition pokemon) team) game

applyStatusEffect :: Game -> Game
applyStatusEffect game = applyStatusEffectAux Gary $ applyStatusEffectAux Ash game

substractedAttack :: PokemonMov -> PokemonMov
substractedAttack attack =
    attack {
      movsLeft = movsLeft attack - 1
    }

substractMovAux :: Pokemon -> Int -> Pokemon
substractMovAux pok attackIndex =
  let
    allMovs = movs pok
    attack =  S.lookup attackIndex allMovs -- equivalente a hacer allMovs !! attackIndex
  in
    case attack of
      Nothing -> pok
      Just att -> pok {
        movs = S.update attackIndex (substractedAttack att) allMovs
    }

substractMovPlayer :: Player -> Int -> Game -> Game
substractMovPlayer player attackNumber game =
  let
    pokemonIndex = getPlayerPokemonIndex player game
    pokemon = getPlayerPokemon player game
    team = getPlayerTeam player game
  in
    updatePlayerTeam player (S.update pokemonIndex (substractMovAux pokemon attackNumber) team) game

substractMov :: Int -> Int -> Game -> Game
substractMov ashMov garyMov game = substractMovPlayer Gary garyMov $ substractMovPlayer Ash ashMov game

--Boost all stats in PokemonStats by mult
recursiveBoost :: Float -> PokemonStatistics -> PokemonStats -> PokemonStatistics
recursiveBoost mult = foldr (`boostStat` mult)

boostPokemon :: Pokemon -> PokemonMov -> Pokemon
boostPokemon pokemon pokemonMov =
  let
    pokemonStats = stats pokemon
    movParameters = movParams pokemonMov
    boostedStats = upgradedStats movParameters
    boostMult = multiplier movParameters
  in
  pokemon { stats = recursiveBoost boostMult pokemonStats boostedStats }

processBuff :: Player -> Pokemon -> PokemonMov -> Game -> Game
processBuff player pokemon pokemonMov game =
  let
    pokemonIndex = getPlayerPokemonIndex player game
    team = getPlayerTeam player game
  in
  updatePlayerTeam player (S.update pokemonIndex (boostPokemon pokemon pokemonMov) team) game

toPositive :: Int -> Int
toPositive x
  | x > 0 = x
  | otherwise = 0

calculateStab :: PokemonAttributes -> PokemonType -> Float
calculateStab attackingAtr attackType = if attackType `elem` attackingAtr then 1.5 else 1

calculateEffectiveness :: PokemonAttributes -> PokemonType -> Float
calculateEffectiveness defendingAtr attackType = product (map (typeTable attackType) defendingAtr)

attackFormula :: Int -> Int -> Int -> Bool -> PokemonAttributes -> PokemonAttributes -> PokemonMov -> Int
attackFormula attack defense defensivePS isCrit attackingAtr defendingAtr pokemonMov =
  let
    attackPower = power (movParams pokemonMov)
    attackType = pokType pokemonMov
    critValue =  if isCrit then 2.0 else 1.0
    attackStab = calculateStab attackingAtr attackType
    efectiveness = calculateEffectiveness defendingAtr attackType
  in
  toPositive(defensivePS - toPositive (floor ((intToFloat(attackPower * attack) * attackStab * efectiveness * critValue / intToFloat defense) / 5.0)))

divideIfBurned :: Maybe PokemonStatus -> Int -> Int
divideIfBurned (Just Burned) attack = multiplyAndFloor attack 0.5
divideIfBurned _ attack = attack

processAttack :: Pokemon -> Pokemon -> PokemonMov -> Float -> Pokemon
processAttack attackingPokemon defensivePokemon pokemonMov probability=
  let
    attackDamageType = dmgType (movParams pokemonMov)
    apStats = stats attackingPokemon
    dpStats = stats defensivePokemon
    attackingPokemonTypes = pokemonType apStats
    defendingPokemonTypes = pokemonType dpStats
    isCrit = probability < crit apStats
    attackingStat = divideIfBurned (status attackingPokemon) (getAttackingStat attackDamageType apStats)
    defensiveStat = getDefensiveStat attackDamageType dpStats
    defensivePS = currentPs dpStats
  in
    defensivePokemon {
      stats = dpStats {
        currentPs = attackFormula attackingStat defensiveStat defensivePS isCrit attackingPokemonTypes defendingPokemonTypes pokemonMov
      }
    }

-- Si ataco Ash el pokemon afectado es el de gary y viceversa
processDmg :: Player-> Pokemon -> Pokemon -> PokemonMov -> Float -> Game -> Game
processDmg player attackingPokemon defendingPokemon pokemonAttack probability game =
  let
    defendingPlayer = otherPlayer player
    defendingPokemonIndex = getPlayerPokemonIndex defendingPlayer game
    defendingTeam = getPlayerTeam defendingPlayer game
  in
  updatePlayerTeam defendingPlayer (S.update defendingPokemonIndex (processAttack attackingPokemon defendingPokemon pokemonAttack probability) defendingTeam) game

applyStatus :: Pokemon -> PokemonMov -> Pokemon
applyStatus defendingPokemon pokemonMov =
    case status defendingPokemon of
      Nothing -> defendingPokemon { status = Just (statusType (movParams pokemonMov))}
      _ -> defendingPokemon

processStatus :: Player-> Pokemon -> PokemonMov -> Game -> Game
processStatus player defendingPokemon pokemonMov game =
  let
    defendingPlayer = otherPlayer player
    defendingPokemonIndex = getPlayerPokemonIndex defendingPlayer game
    defendingTeam = getPlayerTeam defendingPlayer game
  in
  updatePlayerTeam defendingPlayer (S.update defendingPokemonIndex (applyStatus defendingPokemon pokemonMov) defendingTeam) game

checkIfHit :: Float -> PokemonMov -> Bool
checkIfHit prob pokemonMov = prob < accuracy pokemonMov

checkIfParalized :: Float -> Maybe PokemonStatus -> Bool
checkIfParalized prob (Just Paralized) = prob < paralizedChance
checkIfParalized prob _ = True

processMov :: Player -> PokemonMov -> Game -> Game
processMov player pokemonMov game
  | checkIfHit probability pokemonMov && checkIfParalized probability (status attackingPokemon) =
    case movType pokemonMov of
      Buff -> processBuff player attackingPokemon pokemonMov game
      Dmg ->  processDmg player attackingPokemon defendingPokemon pokemonMov probability game
      Status -> processStatus player defendingPokemon pokemonMov game
      Change -> game
  | otherwise = game

  where seed = getSeed game
        probability = randomProbability seed
        currentStatus = status attackingPokemon
        attackingPokemon = getPlayerPokemon player game
        defendingPokemon = getPlayerPokemon (otherPlayer player) game

-- Actualizo a los dos equipos
fight :: PokemonMov -> PokemonMov -> Game -> Game
fight apAttack gpAttack game =
  -- Tengo que eliminar las dos seeds que ya use 
  removeSeed
  $ processMov Gary gpAttack
  $ removeSeed
  $ processMov Ash apAttack game

checkFirstAttack :: Pokemon -> Pokemon -> Game -> Game
checkFirstAttack ashPokemon garyPokemon game
    | ashSpeed > garySpeed = game { firstPlayer = Ash }
    | otherwise = game { firstPlayer = Gary }
    where
      ashSpeed = speed (stats ashPokemon)
      garySpeed = speed (stats garyPokemon)

checkMaybeGaryAttackValue :: Maybe Int -> Int
checkMaybeGaryAttackValue Nothing = 4
checkMaybeGaryAttackValue (Just i) = i

checkMaybeAttack :: Maybe PokemonMov -> PokemonMov
checkMaybeAttack Nothing = PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
checkMaybeAttack (Just pa) = pa

garyAttackPick :: Pokemon -> Int -> Int
garyAttackPick pokemon seed =
  let
    pokemonMovs = movs pokemon
    nonEmptyAttacks = S.filter stillHasMoves pokemonMovs
    randomNumberAttack = randomAttack seed (0, S.length nonEmptyAttacks - 1)
  in
    if S.null nonEmptyAttacks then 5
    else checkMaybeGaryAttackValue (S.elemIndexL (nonEmptyAttacks `S.index` randomNumberAttack) pokemonMovs)

stillHasMoves :: PokemonMov -> Bool
stillHasMoves pokemonMov = movsLeft pokemonMov > 0

battle :: Game -> Int -> Game
battle game ashAttackNumber
    -- | not (isCoordCorrect garyAttackNumber) = game --TODO Sacar el pokemon de Gary
    | stillHasMoves apAttack =
      checkStillFighting
        $ swapDefetedPokemon
        $ applyStatusEffect
        $ substractMov ashAttackNumber garyAttackNumber
        $ fight apAttack gpAttack
        $ checkFirstAttack ashPokemon garyPokemon
        $ removeSeed game
    | otherwise = game
    where
      ashPokemon = getPlayerPokemon Ash game
      garyPokemon = getPlayerPokemon Gary game
      apAttack = checkMaybeAttack (S.lookup ashAttackNumber (movs ashPokemon))
      seed = getSeed game
      garyAttackNumber = garyAttackPick garyPokemon seed
      gpAttack = checkMaybeAttack (S.lookup garyAttackNumber (movs garyPokemon))

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
    | isCoordCorrect attackNumber = battle game attackNumber
    | otherwise = game
    where attackNumber = cellCordToAttack cellCoord


mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = (floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth),
                              floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight))

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> playerTurn game $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (head (getSeeds 1 game))
transformGame _ game = game
