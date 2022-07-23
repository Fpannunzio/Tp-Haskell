module Logic where

import qualified Data.Sequence as S
import qualified Data.Maybe (fromMaybe)

import Graphics.Gloss.Interface.Pure.Game
import Game
import Types
import Random
import Types (Pokemon(currentStatus))
import Game (Game)

simpleEfectiveness :: Float
simpleEfectiveness = 1.0
-- Si el segundo equipo perdio gana el primer jugador siempre
-- Si el primero perdio y no sali por el primer caso es que el segundo no lo es, gana el segundo jugador
-- Sino sigue igual

checkPlayerWon :: Bool -> Bool -> Player -> Player -> Game -> Game
checkPlayerWon _ False firstPlayer _ game = game { gameState = GameOver firstPlayer}
checkPlayerWon False _ _ secondPlayer game = game { gameState = GameOver secondPlayer}
checkPlayerWon _ _ _ _ game = game {gameState = ActionLogging}

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
    pokemonStatus = currentStatus currentPokemon
    pokemonStats = stats currentPokemon
    currentPokemonPs = currentPs pokemonStats
    currentPokemonMaxPs = maxPs pokemonStats
  in
    case pokemonStatus of
      Just Poisoned -> currentPokemon {stats = pokemonStats {currentPs = statusFormula currentPokemonMaxPs currentPokemonPs poisonedPorc}}
      Just Burned -> currentPokemon {stats = pokemonStats {currentPs = statusFormula currentPokemonMaxPs currentPokemonPs burnedPorc}}
      _ -> currentPokemon

logStatusAux :: Player -> Pokemon -> PokemonStatus -> Game -> Game
logStatusAux player pokemon Paralized game = game 
logStatusAux player pokemon status game =
  let
    currentPokemon = getPlayerPokemon player game
    statusLog = ActionLog {playerInvolved = player, pokemonName = name currentPokemon,
    logParams = StatusLogParams {statusLog = status}
    }
  in
    appendLog statusLog game

logStatus :: Player -> Game -> Game
logStatus player game =
  let 
    pokemon = getPlayerPokemon player game
  in
  case currentStatus pokemon of
    Just pokStatus -> logStatusAux player pokemon pokStatus game
    Nothing -> game

applyStatusEffectAux ::Player -> Game -> Game
applyStatusEffectAux player game =
  let
    pokemonIndex = getPlayerPokemonIndex player game
    pokemon = getPlayerPokemon player game
    team = getPlayerTeam player game
  in
    logStatus player $
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
  logAttackDmg player pokemonMov False simpleEfectiveness False $
  updatePlayerTeam player (S.update pokemonIndex (boostPokemon pokemon pokemonMov) team) game

toPositive :: Int -> Int
toPositive x
  | x > 0 = x
  | otherwise = 0

calculateStab :: PokemonAttributes -> PokemonType -> Bool
calculateStab attackingAtr attackType = attackType `elem` attackingAtr

calculateEffectiveness :: PokemonAttributes -> PokemonType -> Float
calculateEffectiveness defendingAtr attackType = product (map (typeTable attackType) defendingAtr)

attackFormula :: Int -> Int -> Int -> Int -> Bool -> Bool -> Float -> Int
attackFormula attackPower attack defense defensivePS isCrit hasStab efectiveness =
  let
    critValue =  if isCrit then 2.0 else 1.0
    attackStab = if hasStab then 1.5 else 1.0
  in
  toPositive(defensivePS - toPositive (floor ((intToFloat(attackPower * attack) * attackStab * efectiveness * critValue / intToFloat defense) / 5.0)))

divideIfBurned :: Maybe PokemonStatus -> Int -> Int
divideIfBurned (Just Burned) attack = multiplyAndFloor attack 0.5
divideIfBurned _ attack = attack

processAttack :: PokemonStatistics -> Maybe PokemonStatus -> PokemonStatistics -> PokemonMov -> Bool -> Bool -> Float -> PokemonStatistics
processAttack apStats apStatus dpStats pokemonMov isCrit hasStab efectiveness =
  let
    attackParams = movParams pokemonMov
    attackDamageType = dmgType attackParams
    attackPower = power (movParams pokemonMov)
    attackingStat = divideIfBurned apStatus (getAttackingStat attackDamageType apStats)
    defensiveStat = getDefensiveStat attackDamageType dpStats
    defensivePS = currentPs dpStats
  in
    dpStats {
        currentPs = attackFormula attackPower attackingStat defensiveStat defensivePS isCrit hasStab efectiveness
    }
    

logAttackDmg :: Player -> PokemonMov -> Bool -> Float-> Bool -> Game -> Game
logAttackDmg player pokemonMov attackFailedLog effectivenessLog critLog game =
  let
    currentPokemon = getPlayerPokemon player game
    attackLog = ActionLog {playerInvolved = player, pokemonName = name currentPokemon,
    logParams = AttackLogParams {attackLogged = attackName pokemonMov, attackFailed = attackFailedLog, effectiveness = effectivenessLog, isCrit = critLog}
    }
  in
    appendLog attackLog game

-- Si ataco Ash el pokemon afectado es el de gary y viceversa
processDmg :: Player-> Pokemon -> Pokemon -> PokemonMov -> Float -> Game -> Game
processDmg player attackingPokemon defendingPokemon pokemonAttack probability game =
  let
    defendingPlayer = otherPlayer player
    defendingPokemonIndex = getPlayerPokemonIndex defendingPlayer game
    defendingTeam = getPlayerTeam defendingPlayer game
    apStats = stats attackingPokemon
    apStatus = currentStatus attackingPokemon
    dpStats = stats defendingPokemon
    attackType = pokType pokemonAttack
    attackingPokemonTypes = pokemonType apStats
    defendingPokemonTypes = pokemonType dpStats
    isCrit = probability < crit apStats
    hasStab = calculateStab attackingPokemonTypes attackType
    efectiveness = calculateEffectiveness defendingPokemonTypes attackType
  in
    logAttackDmg player pokemonAttack False efectiveness isCrit $
    updatePlayerTeam defendingPlayer (S.update defendingPokemonIndex (defendingPokemon{stats = processAttack apStats apStatus dpStats pokemonAttack isCrit hasStab efectiveness}) defendingTeam) game

applyStatus :: Pokemon -> PokemonMov -> Pokemon
applyStatus defendingPokemon pokemonMov =
    case currentStatus defendingPokemon of
      Nothing -> defendingPokemon { currentStatus = Just (statusType (movParams pokemonMov))}
      _ -> defendingPokemon

processStatus :: Player-> Pokemon -> PokemonMov -> Game -> Game
processStatus player defendingPokemon pokemonMov game =
  let
    defendingPlayer = otherPlayer player
    defendingPokemonIndex = getPlayerPokemonIndex defendingPlayer game
    defendingTeam = getPlayerTeam defendingPlayer game
  in
  logAttackDmg player pokemonMov False simpleEfectiveness False $
  updatePlayerTeam defendingPlayer (S.update defendingPokemonIndex (applyStatus defendingPokemon pokemonMov) defendingTeam) game

checkIfHit :: Float -> PokemonMov -> Bool
checkIfHit prob pokemonMov = prob < accuracy pokemonMov

checkIfParalized :: Float -> Maybe PokemonStatus -> Bool
checkIfParalized prob (Just Paralized) = prob < paralizedChance
checkIfParalized prob _ = True

logParalized :: Player -> Game -> Game
logParalized player game =
  let
    currentPokemon = getPlayerPokemon player game
    changeLog = ActionLog {playerInvolved = player, pokemonName = name currentPokemon, logParams = StatusLogParams {statusLog = Paralized}}
  in
    appendLog changeLog game

processMov :: Player -> PokemonMov -> Game -> Game
processMov player pokemonMov game
  | checkIfHit probability pokemonMov && paralized =
    case movType pokemonMov of
      Buff -> processBuff player attackingPokemon pokemonMov game
      Dmg ->  processDmg player attackingPokemon defendingPokemon pokemonMov probability game
      Status -> processStatus player defendingPokemon pokemonMov game
  | not (checkIfHit probability pokemonMov) =
    logAttackDmg player pokemonMov False simpleEfectiveness False game
  | not paralized =
    logParalized player game
  | otherwise = game

  where seed = getSeed game
        probability = randomProbability seed
        paralized = checkIfParalized probability (currentStatus attackingPokemon)
        attackingPokemon = getPlayerPokemon player game
        defendingPokemon = getPlayerPokemon (otherPlayer player) game
-- Actualizo a los dos equipos
fight :: Player -> PokemonMov -> Game -> Game
fight player apAttack game =
  -- Tengo que eliminar la seed que use
  removeSeed
  $ processMov player apAttack game

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
      $ fight Gary gpAttack
      $ fight Ash apAttack
      $ checkFirstAttack ashPokemon garyPokemon
      $ resetActions 
      $ removeSeed game
  | otherwise = game
  where
    ashPokemon = getPlayerPokemon Ash game
    garyPokemon = getPlayerPokemon Gary game
    apAttack = checkMaybeAttack (S.lookup ashAttackNumber (movs ashPokemon))
    seed = getSeed game
    garyAttackNumber = garyAttackPick garyPokemon seed
    gpAttack = checkMaybeAttack (S.lookup garyAttackNumber (movs garyPokemon))

logChange :: Pokemon -> Game -> Game
logChange oldPokemon game =
  let
    gameActions = actions game
    newPokemon = getPlayerPokemon Ash game
    changeLog = ActionLog {playerInvolved = Ash, pokemonName = name newPokemon, logParams = ChangeLogParams {previousPokemon = name oldPokemon} }
  in
    appendLog changeLog game

resetActions :: Game -> Game
resetActions game = game {actions = []}

change :: Game -> Int -> Game
change game changeNumber =
  let
    seed = getSeed game
    ashOldPokemon = getPlayerPokemon Ash game
    garyPokemon = getPlayerPokemon Gary game
    garyAttackNumber = garyAttackPick garyPokemon seed
    gpAttack = checkMaybeAttack (S.lookup garyAttackNumber (movs garyPokemon))
    newGame = game {firstPlayer = Gary, ashPokemon = changeNumber}
  in
    checkStillFighting
    $ applyStatusEffect
    $ substractMov 7 garyAttackNumber
    $ fight Gary gpAttack
    $ logChange ashOldPokemon
    $ resetActions 
    $ removeSeed newGame

isMovCorrect :: Int -> Bool
isMovCorrect x = x >= 0 && x <=3 -- inRange?

isChangeCorrect :: Int -> Game -> Bool
isChangeCorrect changeNumber game =
  let
    playerTeam = ashTeam game
  in
  changeNumber >= 0 && changeNumber <=5 && not (checkDefeated (playerTeam `S.index` changeNumber))

cellCordToAttack :: (Int, Int) -> Int
cellCordToAttack cellCord =
  case cellCord of
    (0, 1) -> 0
    (1, 1) -> 1
    (0, 0) -> 2
    (1, 0) -> 3
    _ -> 4

processAction:: Game -> Action -> Game
processAction game (Movement movNumber) = if isMovCorrect movNumber then battle game movNumber else game
processAction game (Change changeNumber) = if isChangeCorrect changeNumber game then change game changeNumber else game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = (floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth),
                              floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight))

-- validChar :: Char -> Bool
-- validChar c = 

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> processAction game $ Movement $ cellCordToAttack $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game {gameState = Running}
transformGame (EventKey (Char '1') Up _ _) game =
    case gameState game of
      Running -> processAction game $ Change 0
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
transformGame (EventKey (Char '2') Up _ _) game =
    case gameState game of
      Running -> processAction game $ Change 1
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
transformGame (EventKey (Char '3') Up _ _) game =
    case gameState game of
      Running -> processAction game $ Change 2
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
transformGame (EventKey (Char '4') Up _ _) game =
    case gameState game of
      Running -> processAction game $ Change 3
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
transformGame (EventKey (Char '5') Up _ _) game =
    case gameState game of
      Running -> processAction game $ Change 4
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
transformGame (EventKey (Char '6') Up _ _) game =
    case gameState game of
      Running -> processAction game $ Change 5
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
      -- TODO paramertrizr char
-- transformGame (EventKey (Char c) Up _ _) game =
--     case gameState game of
--       Running -> if validChar c then processAction game $ Change charToChange else game
--       GameOver _ -> initialGame (head (getSeeds 1 game))
--       _ -> game
transformGame _ game = game
    -- case gameState game of
    --   InitialScreen -> game {gameState = Running}
    --   _ -> game