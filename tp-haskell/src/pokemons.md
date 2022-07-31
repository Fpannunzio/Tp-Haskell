Pokemon {
    name = "Bulbasaur"
  , pokedexNumber = 1
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 0.1, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Hoja afilada", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Electrico, movParams = StatusMov {statusType = Paralized}}]
  }
  ,Pokemon {
    name = "Pikachu"
  , pokedexNumber = 11
  , stats = PokemonStatistics {pokemonType = [Electrico], maxPs = 100, currentPs = 100, attack = 5, defense = 7, spAttack = 15, spDefense = 5, speed = 30, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Impactrueno", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Electrico, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Sobrecarga", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Electrico, movParams = BuffMov {multiplier = 2.0, upgradedStats = [SpecialAttack, Speed]}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Electrico, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Lapras"
  , pokedexNumber = 18
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 130, currentPs = 130, attack = 35, defense = 40, spAttack = 60, spDefense = 40, speed = 25, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Surf", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Paz Mental", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}]
  },
  Pokemon {
    name = "Venusaur"
  , pokedexNumber = 3
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 180, currentPs = 180, attack = 25, defense = 30, spAttack = 50, spDefense = 30, speed = 15, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Desarrollar", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 5, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Terremoto", movType = Dmg, movsLeft = 5, accuracy = 0.7, pokType = Tierra, movParams = DmgMov {power = 75, dmgType = Physic}}
  },
  Pokemon {
    name = "Snorlax"
  , pokedexNumber = 19
  , stats = PokemonStatistics {pokemonType = [Normal], maxPs = 180, currentPs = 180, attack = 45, defense = 50, spAttack = 10, spDefense = 50, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Hiperrayo", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 100, dmgType = Physic}}
  , PokemonMov {attackName = "Panzazo", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 65, dmgType = Physic}}
  , PokemonMov {attackName = "Paz Mental", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}]
  },
  Pokemon {
    name = "Charizard"
  , pokedexNumber = 6
  , stats = PokemonStatistics {pokemonType = [Fuego, Volador], maxPs = 130, currentPs = 100, attack = 85, defense = 20, spAttack = 50, spDefense = 20, speed = 25, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 0.8, pokType = Fuego, movParams = DmgMov {power = 75, dmgType = Physic}}
  , PokemonMov {attackName = "Golpe Ala", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Volador , movParams = DmgMov {power = 55, dmgType = Physic}}
  , PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Volador, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  }]

generatePokemonTeamGary :: PokemonTeam
generatePokemonTeamGary = S.fromList [
,
  Pokemon {
    name = "Arcanine"
  , pokedexNumber = 12
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 45, defense = 15, spAttack = 25, spDefense = 15, speed = 40, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Bola Fuego", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 35, dmgType = Physic}}
  , PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 55, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Onyx"
  , pokedexNumber = 15
  , stats = PokemonStatistics {pokemonType = [Tierra, Roca], maxPs = 150, currentPs = 150, attack = 45, defense = 40, spAttack = 20, spDefense = 20, speed = 15, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Terremoto", movType = Dmg, movsLeft = 5, accuracy = 0.7, pokType = Tierra, movParams = DmgMov {power = 75, dmgType = Physic}}
  , PokemonMov {attackName = "Pedrada", movType = Dmg, movsLeft = 5, accuracy = 1.0, pokType = Roca, movParams = DmgMov {power = 45, dmgType = Physic}}
  , PokemonMov {attackName = "Defensa ferrea", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Tierra, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense]}}
  , PokemonMov {attackName = "Noquear", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Tierra, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Gyarados"
  , pokedexNumber = 17
  , stats = PokemonStatistics {pokemonType = [Agua, Volador], maxPs = 130, currentPs = 130, attack = 65, defense = 25, spAttack = 30, spDefense = 25, speed = 20, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Volador, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Precision", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 4.0, upgradedStats = [Crit]}}]
  },
  Pokemon {
    name = "Pidgeot"
  , pokedexNumber = 10
  , stats = PokemonStatistics {pokemonType = [Volador, Normal], maxPs = 120, currentPs = 120, attack = 35, defense = 20, spAttack = 15, spDefense = 20, speed = 40, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Golpe Ala", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Volador , movParams = DmgMov {power = 55, dmgType = Physic}}
  , PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Volador, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Electrico, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Blastoise"
  , pokedexNumber = 9
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 140, currentPs = 140, attack = 65, defense = 35, spAttack = 50, spDefense =25, speed = 40, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Surf", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Precision", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 4.0, upgradedStats = [Crit]}}]
  }]

 ,Pokemon {
    name = "Charmander"
  , pokedexNumber = 4
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
Pokemon {
    name = "Charmilion"
  , pokedexNumber = 5
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Ascuas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
 Pokemon {
    name = "Charizard"
  , pokedexNumber = 6
  , stats = PokemonStatistics {pokemonType = [Fuego, Volador], maxPs = 130, currentPs = 100, attack = 85, defense = 20, spAttack = 50, spDefense = 20, speed = 25, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 0.8, pokType = Fuego, movParams = DmgMov {power = 75, dmgType = Physic}}
  , PokemonMov {attackName = "Golpe Ala", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Volador , movParams = DmgMov {power = 55, dmgType = Physic}}
  , PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Volador, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  }
Pokemon {
    name = "Squirtle"
  , pokedexNumber = 7
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Burbuja", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Hidrochorro", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Physic}}]
  }
,Pokemon {
    name = "Wartortle"
  , pokedexNumber = 8
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Burbuja", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Burned}}]
  },
  Pokemon {
    name = "Blastoise"
  , pokedexNumber = 9
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 140, currentPs = 140, attack = 65, defense = 35, spAttack = 50, spDefense =25, speed = 40, crit = 0.05}
  , currentStatus = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Surf", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Agua, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Hidro bomba", movType = Dmg, movsLeft = 5, accuracy = 0.75, pokType = Agua, movParams = DmgMov {power = 85, dmgType = Physic}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Precision", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 4.0, upgradedStats = [Crit]}}]
  }