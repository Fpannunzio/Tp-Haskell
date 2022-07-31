module Logic where

import qualified Data.Sequence as S
import qualified Data.Maybe (fromMaybe)

import Graphics.Gloss.Interface.Pure.Game
import Game
import Types
import Random
import Data.Char(isDigit, digitToInt)

simpleEfectiveness :: Float
simpleEfectiveness = 1.0

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
      logChange player True pokemon
      $ updatePlayerCurrentPokemon player firstAlivePokemon game
    else if checkDefeated pokemon && firstAlivePokemon >= invalidPokemon then
      game { gameState = GameOver (otherPlayer player), currentPlayer = player}
    else
      game {currentPlayer = player}

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
    swapPlayerDefeatedPokemon player
    $ logStatus player
    $ updatePlayerTeam player (S.update pokemonIndex (applyStatusCondition pokemon) team) game

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
  logAttackDmg player pokemonMov False False simpleEfectiveness $
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
  toPositive(defensivePS - toPositive (floor ((fromIntegral(attackPower * attack) * attackStab * efectiveness * critValue / fromIntegral defense) / 5.0)))

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


logAttackDmg :: Player -> PokemonMov -> Bool -> Bool -> Float -> Game -> Game
logAttackDmg player pokemonMov attackFailedLog critLog effectivenessLog game =
  let
    currentPokemon = getPlayerPokemon player game
    attackLog = ActionLog {playerInvolved = player, pokemonName = name currentPokemon,
    logParams = AttackLogParams {attackLogged = attackName pokemonMov, attackFailed = attackFailedLog, isCrit = critLog, effectiveness = effectivenessLog}
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
    logAttackDmg player pokemonAttack False isCrit efectiveness $
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
  logAttackDmg player pokemonMov False False simpleEfectiveness $
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
      Buff -> processBuff player attackingPokemon pokemonMov newGame
      Dmg ->  processDmg player attackingPokemon defendingPokemon pokemonMov probability newGame
      Status -> processStatus player defendingPokemon pokemonMov newGame
  | not (checkIfHit probability pokemonMov) =
    logAttackDmg player pokemonMov True False simpleEfectiveness newGame
  | not paralized =
    logParalized player newGame
  | otherwise = newGame

  where (seed, newGame) = getSeed game
        probability = randomProbability seed
        paralized = checkIfParalized probability (currentStatus attackingPokemon)
        attackingPokemon = getPlayerPokemon player newGame
        defendingPokemon = getPlayerPokemon (otherPlayer player) newGame
-- Actualizo a los dos equipos
fight :: Player -> PokemonMovPair -> Game -> Game
fight player movPair game
  | currentPlayer game == player =
    substractMovPlayer player (fst movPair)
    $ swapPlayerDefeatedPokemon (otherPlayer player)
    $ processMov player (snd movPair) game
  | otherwise = game

checkFirstAttack :: Pokemon -> Pokemon -> Game ->(Player, Game)
checkFirstAttack ashPokemon garyPokemon game
    | ashSpeed > garySpeed = (Ash, game { firstPlayer = Ash, currentPlayer = Ash})
    | otherwise = (Gary ,game { firstPlayer = Gary, currentPlayer = Gary })
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

logChange :: Player -> Bool -> Pokemon -> Game -> Game
logChange player isDefeated oldPokemon game =
  let
    gameActions = actions game
    newPokemon = getPlayerPokemon player game
    changeLog = ActionLog {playerInvolved = player, pokemonName = name newPokemon, logParams = ChangeLogParams {defeated = isDefeated, previousPokemon = name oldPokemon} }
  in
    appendLog changeLog game

resetActions :: Game -> Game
resetActions game = game {actions = []}

getAttacks :: PokemonMovPair -> PokemonMovPair -> Game -> (PokemonMovPair, PokemonMovPair)
getAttacks ashAttackPair garyAttackPair game
  | firstPlayer game == Ash = (ashAttackPair, garyAttackPair)
  | otherwise = (garyAttackPair, ashAttackPair)

setActionLogging :: Game -> Game
setActionLogging game = if gameState game == Running then game {gameState = ActionLogging} else game

change :: Int -> Player -> Game -> Game
change changeNumber player game =
  let 
    oldPokemon = getPlayerPokemon player game
  in
  logChange player False oldPokemon
  $ case player of
  Ash -> game {firstPlayer = Gary, currentPlayer = Gary, ashPokemon = changeNumber}
  Gary -> game {firstPlayer = Ash, currentPlayer = Ash, ashPokemon = changeNumber} 
  
actionChange :: Int -> Player -> PokemonMovPair -> Game -> Game
actionChange changeNumber player movPair game = fight (otherPlayer player) movPair $ change changeNumber player game

actionFight :: Int -> PokemonMovPair -> Game -> Game
actionFight ashMovNumber garyAttackPair game =
  let 
    garyPokemon = getPlayerPokemon Gary game
    ashPokemon = getPlayerPokemon Ash game
    apAttack = checkMaybeAttack (S.lookup ashMovNumber (movs ashPokemon))
    (fastestPlayer, newGame) = checkFirstAttack ashPokemon garyPokemon game
    (fMovPair, sMovPair) = getAttacks (ashMovNumber, apAttack) garyAttackPair newGame
  in
  fight (otherPlayer fastestPlayer) sMovPair
  $ fight fastestPlayer fMovPair newGame

battle :: Game -> Action -> Game
battle game action =
  let
    (seed, removedSeedGame) = getSeed game
    garyPokemon = getPlayerPokemon Gary removedSeedGame
    garyAttackNumber = garyAttackPick garyPokemon seed
    gpAttack = checkMaybeAttack (S.lookup garyAttackNumber (movs garyPokemon))
  in
    setActionLogging
    $ applyStatusEffect
    $ case action of
      (Movement movNumber) -> actionFight movNumber (garyAttackNumber, gpAttack)
      (Change changeNumber) -> actionChange changeNumber Ash (garyAttackNumber, gpAttack)
    $ resetActions removedSeedGame

isMovCorrect :: Int -> Game -> Bool
isMovCorrect movNumber game = 
  let 
    ashPokemon = getPlayerPokemon Ash game
    apAttack = checkMaybeAttack (S.lookup movNumber (movs ashPokemon))
  in
   movNumber >= 0 && movNumber <=3 && stillHasMoves apAttack

isChangeCorrect :: Int -> Game -> Bool
isChangeCorrect changeNumber game =
  let
    playerTeam = ashTeam game
    currentPokemon = ashPokemon game
  in
  changeNumber >= 0 && changeNumber <=5 && changeNumber /= currentPokemon && not (checkDefeated (playerTeam `S.index` changeNumber))

cellCordToAttack :: (Int, Int) -> Int
cellCordToAttack cellCord =
  case cellCord of
    (0, 1) -> 0
    (1, 1) -> 1
    (0, 0) -> 2
    (1, 0) -> 3
    _ -> 4

validAction :: Action -> Game -> Bool
validAction (Change changeNumber) game = isChangeCorrect changeNumber game
validAction (Movement movNumber) game = isMovCorrect movNumber game

processAction:: Game -> Action -> Game
processAction game action = if validAction action game then battle game action else game

mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = (floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth),
                              floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight))

transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) game =
    case gameState game of
      Running -> processAction game $ Movement $ cellCordToAttack $ mousePosAsCellCoord mousePos
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game {gameState = Running}
transformGame (EventKey (Char c) Up _ _) game =
    case gameState game of
      Running -> if isDigit c then processAction game $ Change (digitToInt c - 1) else game
      GameOver _ -> initialGame (head (getSeeds 1 game))
      _ -> game
transformGame _ game = game
