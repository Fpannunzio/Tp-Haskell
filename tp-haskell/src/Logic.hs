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

applyStatusEffect :: Game -> Game
applyStatusEffect game =
  let
    ashPokemon = head (ashTeam game)
    garyPokemon = head (garyTeam game)
  in
    game {
      ashTeam = applyStatusCondition ashPokemon : tail (ashTeam game)
    , garyTeam = applyStatusCondition garyPokemon : tail (garyTeam game)
    }

substractedAttack :: PokemonMov -> PokemonMov
substractedAttack attack =
    attack {
      movsLeft = movsLeft attack - 1
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

--Boost all stats in PokemonStats by mult
recursiveBoost :: Float -> PokemonStatistics -> PokemonStats -> PokemonStatistics
recursiveBoost mult = foldr (`boostStat` mult)

boostStats :: Pokemon -> PokemonMov -> Pokemon
boostStats boostedPokemon pokemonMov =
  let
    pokemonStats = stats boostedPokemon
    attackParams = movParams pokemonMov
    boostedStats = upgradedStats attackParams
    boostMult = multiplier attackParams
  in
  boostedPokemon {
    stats = recursiveBoost boostMult pokemonStats boostedStats
  }

processBuff :: Player -> Pokemon -> PokemonMov -> Game -> Game
processBuff Ash attackingPokemon pokemonMov game = game { ashTeam = boostStats attackingPokemon pokemonMov : tail (ashTeam game) }
processBuff Gary attackingPokemon pokemonMov game = game { garyTeam = boostStats attackingPokemon pokemonMov : tail (garyTeam game) }

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
processAttack attackingPokemon defensivePokemon pokemonMov probability =
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
processDmg Ash attackingPokemon defendingPokemon pokemonAttack probability game = game { garyTeam = processAttack attackingPokemon defendingPokemon pokemonAttack probability : tail (garyTeam game) }
processDmg Gary attackingPokemon defendingPokemon pokemonAttack probability game = game { ashTeam = processAttack attackingPokemon defendingPokemon pokemonAttack probability : tail (ashTeam game) }


applyStatus :: Pokemon -> PokemonMov -> Pokemon
applyStatus defendingPokemon pokemonAttack =
    case status defendingPokemon of
      Nothing -> defendingPokemon { status = Just (statusType (movParams pokemonAttack))}
      _ -> defendingPokemon

processStatus :: Player-> Pokemon -> PokemonMov -> Game -> Game
processStatus Ash defendingPokemon pokemonAttack game = game { garyTeam = applyStatus defendingPokemon pokemonAttack : tail (garyTeam game) }
processStatus Gary defendingPokemon pokemonAttack game = game { ashTeam = applyStatus defendingPokemon pokemonAttack : tail (ashTeam game) }

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

checkMaybeGaryValue :: Maybe Int -> Int
checkMaybeGaryValue Nothing = 4
checkMaybeGaryValue (Just i) = i

checkMaybeAttack :: Maybe PokemonMov -> PokemonMov
checkMaybeAttack Nothing = PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
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

stillHasMoves :: PokemonMov -> Bool
stillHasMoves pokemonMov = movsLeft pokemonMov > 0

battle :: Game -> Int -> Game
battle game attackNumber
    -- | not (isCoordCorrect garyAttackNumber) = game --TODO Sacar el pokemon de Gary
    | stillHasMoves apAttack =
      checkStillFighting
        $ swapDefetedPokemon
        $ applyStatusEffect
        $ substractAttackMov attackNumber garyAttackNumber
        $ fight apAttack gpAttack
        $ checkFirstAttack ashPokemon garyPokemon
        $ removeSeed game
    | otherwise = game
    where
      ashPokemon = head (ashTeam game)
      garyPokemon = head (garyTeam game)
      apAttack = checkMaybeAttack (Data.Sequence.lookup attackNumber (movs ashPokemon))
      seed = getSeed game
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
    | isCoordCorrect attackNumber = battle game attackNumber
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
