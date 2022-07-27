module Rendering where

import Data.Array
import Data.Foldable (toList)
import qualified Data.Sequence as S
import Data.Maybe

import Graphics.Gloss

import Game
import Types
import Colors
import Positions

type Sprites = S.Seq Picture

type Scale = (Float, Float)

defaultTextScale :: Scale
defaultTextScale = (0.3, 0.3)

attackTextScale :: Scale
attackTextScale = (0.4, 0.4)

logoImgScale :: Scale
logoImgScale = (0.8, 0.8)

defaultImgScale :: Scale
defaultImgScale = (3.0, 3.0)

firstInstructions :: String
firstInstructions = "Click on the attack you want to use"

secondInstructions :: String
secondInstructions = "or use [1-6] to change your pokemon"

thirdInstructions :: String
thirdInstructions = "Click anywhere to continue"

renderGameLogo :: Picture -> Picture
renderGameLogo logo = uncurry translate gameLogoPosition $ uncurry scale logoImgScale logo

renderInstructions :: String -> Position -> Picture
renderInstructions instruction insPosition =
  uncurry translate insPosition
  $ uncurry scale defaultTextScale
  $ color textColor
  $ text instruction

renderInitialState ::Picture -> Picture
renderInitialState gameLogo = pictures [
  renderGameLogo gameLogo,
  renderInstructions firstInstructions firstInstructionsPosition,
  renderInstructions secondInstructions secondInstructionsPosition,
  renderInstructions thirdInstructions thirdInstructionsPosition]

renderPokeball :: Picture
renderPokeball = pictures [
  circle (fromIntegral screenWidth * 0.01)
  , circle (fromIntegral screenWidth * 0.005)]

getPokemonSprite :: Sprites -> Pokemon -> Picture
getPokemonSprite sprites pokemon =
  Data.Maybe.fromMaybe
  Blank (S.lookup (pokedexNumber pokemon - 1) sprites)

-- Deberia mostrar, las dos fotos de los pokemones, su nombre, su vida y los ataques para elegir
gameRunningPicture :: Sprites -> Sprites -> Game -> Picture
gameRunningPicture ashSprites garySprites game =
    pictures [
              renderPokemon Gary garyPokemon garyPokemonSprite
            , renderPokemon Ash ashPokemon ashPokemonSprite
            , renderAlivePokemon Gary garyTeam
            , renderAlivePokemon Ash ashTeam
            , pokemonAttacksBoard (movs ashPokemon)
            , color boardGridColor boardGrid
             ]
    where
          ashPokemon = getPlayerPokemon Ash game
          garyPokemon = getPlayerPokemon Gary game
          ashTeam = getPlayerTeam Ash game
          garyTeam = getPlayerTeam Gary game
          ashPokemonSprite = getPokemonSprite ashSprites ashPokemon
          garyPokemonSprite = getPokemonSprite garySprites garyPokemon

renderEffectiveness :: Float -> String
renderEffectiveness effectiveness 
  | effectiveness == 4.0 = " es superefectivo."
  | effectiveness == 2.0 = " es muy efectivo."
  | effectiveness == 0.5 = " es poco efectvo."
  | effectiveness == 0.25 = "es muy poco efectivo."
  | otherwise = ""

renderCrit :: Bool -> String
renderCrit True = " Un golpe critico!"
renderCrit False = ""

renderAttackLog :: String -> String -> Bool -> Bool -> Float -> Picture
renderAttackLog pokemonName attack failed crit effectivenessValue =
    text $
    pokemonName ++ " uso " ++ attack ++
    (if failed then " but failed."
    else renderEffectiveness effectivenessValue ++ renderCrit crit)

renderChangingLog :: Player -> String -> String -> Bool -> Picture 
renderChangingLog player pokemonName oldPokemonName False = 
  text $ show player ++ " cambia a " ++ oldPokemonName ++ ". Es el turno de " ++ pokemonName
renderChangingLog player pokemonName oldPokemonName True =
    text $  oldPokemonName ++ " de " ++ show player ++ " ya no puede seguir. Sale " ++ pokemonName

renderStatusLog ::  String -> PokemonStatus -> Picture 
renderStatusLog pokemonName status =
    text $
    case status of
      Paralized -> pokemonName ++ " no puede moverse debido a la Paralisis."
      Burned -> pokemonName ++ " se resiente por las quemaduras"
      Poisoned -> "El veneno resta vida a " ++ pokemonName

renderActionLog :: Float -> ActionLog -> Picture
renderActionLog height action =
  let
    player = playerInvolved action
    playerPos = playerPosition player
    pokemon = pokemonName action
    logParameters = logParams action
  in
    translate 0 height
    $ uncurry scale defaultTextScale
    $ color (playerColor player)
    $ case logParameters of
      AttackLogParams attackName attackMissed efect crit-> renderAttackLog pokemon attackName attackMissed crit efect
      ChangeLogParams defeated previousPokemon-> renderChangingLog player pokemon previousPokemon defeated
      StatusLogParams status-> renderStatusLog pokemon status

renderGameActions :: Float -> [ActionLog] -> [Picture]
renderGameActions _ []  = []
renderGameActions position (aLog:aLogs)  = renderActionLog position aLog : renderGameActions (position - logPosition) aLogs 

renderBottomPicture :: State -> Game -> Picture
renderBottomPicture Running game = pictures [ 
              pokemonAttacksBoard (movs (getPlayerPokemon Ash game))
            , color boardGridColor boardGrid]
renderBottomPicture ActionLogging game = pictures [
              pictures $ renderGameActions logHeight (reverse (actions game))
            , color boardGridColor divisionLine]

-- Deberia mostrar, las dos fotos de los pokemones, su nombre, su vida y los ataques para elegir
gamePicture :: State -> Sprites -> Sprites -> Game -> Picture
gamePicture state ashSprites garySprites game =
    pictures [
              renderPokemon Gary garyPokemon garyPokemonSprite
            , renderPokemon Ash ashPokemon ashPokemonSprite
            , renderAlivePokemon Gary garyTeam
            , renderAlivePokemon Ash ashTeam
            , renderBottomPicture state game
             ]
    where
          ashPokemon = getPlayerPokemon Ash game
          garyPokemon = getPlayerPokemon Gary game
          ashTeam = getPlayerTeam Ash game
          garyTeam = getPlayerTeam Gary game
          ashPokemonSprite = getPokemonSprite ashSprites ashPokemon
          garyPokemonSprite = getPokemonSprite garySprites garyPokemon

renderPokemonName :: Float -> Color -> String -> Picture
renderPokemonName position currentColor name =
    translate position nameHeight
    $ uncurry scale defaultTextScale
    $ color currentColor
    $ text name

statusText :: PokemonStatus -> String
statusText Poisoned = "ENV"
statusText Paralized = "PAR"
statusText Burned = "QUE"

renderPokemonStatus :: Float -> Maybe PokemonStatus -> Picture
renderPokemonStatus position Nothing = Blank
renderPokemonStatus position (Just status) =
  translate position nameHeight
  $ uncurry scale defaultTextScale
  $ color (pokemonStatusColor status)
  $ text (statusText status)

calculateLifeBarLength :: Float -> Int -> Int -> Float
calculateLifeBarLength initPosition currentPs maxPs = initPosition + lifeBarMaxLength * fromIntegral currentPs / fromIntegral maxPs

lifeString :: Int -> Int -> String
lifeString currentPs maxPs = show currentPs ++ "/" ++ show maxPs

calculateLifePointsPosition :: Float -> Float
calculateLifePointsPosition initPosition = initPosition + lifeBarMaxLength * 1.1

renderPokemonLife :: Float -> Int -> Int -> Picture
renderPokemonLife position currentPs maxPs =
  pictures [
  color lifeColor
  $ line [(position, lifeHeight), (calculateLifeBarLength position currentPs maxPs, lifeHeight)]
  ,
  translate (calculateLifePointsPosition position) lifeHeight
  $ uncurry scale defaultTextScale
  $ color lifeColor
  $ text (lifeString currentPs maxPs)
  ]

renderPokemonSprite :: Float -> Picture -> Picture
renderPokemonSprite position sprite =
  uncurry translate (position, spriteHeight)
  $ uncurry scale defaultImgScale sprite

renderPokemon :: Player -> Pokemon -> Picture -> Picture
renderPokemon player pokemon sprite = pictures [
      renderPokemonName (playerPosition player) (playerColor player) (name pokemon)
    , renderPokemonStatus (playerPosition player + statusPosition) (currentStatus pokemon)
    , renderPokemonLife (playerPosition player) (currentPs (stats pokemon)) (maxPs (stats pokemon))
    , renderPokemonSprite (playerPosition player + spritePosition) sprite 
  ]

drawPokeball :: Pokemon -> Picture
drawPokeball pokemon =
  if currentPs (stats pokemon) > 0 then Color (makeColorI 255 0 0 255) renderPokeball
  else Color (greyN 0.5) renderPokeball

recursiveTeamRendering :: [Pokemon] -> Float -> [Picture]
recursiveTeamRendering [] position = []
recursiveTeamRendering (pok:pokTeam) position = uncurry translate (position, pokeballHeight) (drawPokeball pok) : recursiveTeamRendering pokTeam (position + pokeballPosition)

renderAlivePokemon :: Player -> PokemonTeam -> Picture
renderAlivePokemon player team =
  let
    position = playerPosition player
  in
    pictures (recursiveTeamRendering (toList team) (position + (pokeballPosition / 2)))

-- Linea divisora de la pantalla
divisionLine :: Picture
divisionLine = line [ (0.0, cellHeight * 2)
            , (fromIntegral screenWidth , middleOfScreen)]

boardGrid :: Picture
boardGrid =
    pictures
     [ line [ (cellWidth, 0.0)
            , (cellWidth, cellHeight * 2) -- Linea |
            ]
    , line [ (0.0, 0.0)
            , (fromIntegral screenWidth, 0.0) -- Linea __
            ]
    , line [ (0.0, cellHeight)
            , (fromIntegral screenWidth, cellHeight) -- Linea --
            ]
    , divisionLine
    ]

attackString :: Int -> String -> String
attackString movCount attName = show movCount ++ "-" ++ attName

renderPokemonAttack :: PokemonMov -> Position -> Picture
renderPokemonAttack pokemonAttack pos =
  let
    movCount = movsLeft pokemonAttack
    attName = attackName pokemonAttack
  in
  uncurry translate pos 
  $ uncurry scale attackTextScale 
  $ color (pokemonTypeColor (pokType pokemonAttack))
  $ Text (attackString movCount attName)

-- Translate te pone el pixel abajo
-- IMPORTANTE primero escalar y despues traducir
pokemonAttacksBoard :: PokemonMovs -> Picture
pokemonAttacksBoard pokeMovs = pictures $ toList (S.zipWith renderPokemonAttack pokeMovs attackPositions)

renderWinner :: Player -> Picture
renderWinner winner = uncurry translate winnerPosition
                     $ color (playerColor winner)
                     $ text (show winner ++ " wins")

gameAsPicture :: Picture -> Sprites -> Sprites -> Game -> Picture
gameAsPicture gameLogo ashSprites garySprites game = translate (fromIntegral screenWidth * (-0.5))
                               (fromIntegral screenHeight * (-0.5))
                               frame
    where frame = case gameState game of
                    InitialScreen -> renderInitialState gameLogo
                    Running -> gamePicture Running ashSprites garySprites game
                    ActionLogging -> gamePicture ActionLogging ashSprites garySprites game
                    GameOver winner -> renderWinner winner
