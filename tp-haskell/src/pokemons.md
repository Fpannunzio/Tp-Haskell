Pokemon {
    name = "Bulbasaur"
  , pokedexNumber = 1
  , stats = PokemonStatistics {pokemonType = [Hierba], maxPs = 100, currentPs = 100, attack = 15, defense = 7, spAttack = 6, spDefense = 5, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Placaje", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 25, dmgType = Physic}}
  , PokemonMov {attackName = "Hoja afilada", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 25, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  }
  
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
    name = "Lapras"
  , pokedexNumber = 24
  , stats = PokemonStatistics {pokemonType = [Agua], maxPs = 130, currentPs = 130, attack = 35, defense = 40, spAttack = 60, spDefense = 40, speed = 25, crit = 0.05}
  , status = Nothing
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
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Desarrollar", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Rayo Solar", movType = Dmg, movsLeft = 5, accuracy = 1.0, pokType = Hierba, movParams = DmgMov {power = 65, dmgType = Special}}
  , PokemonMov {attackName = "Envenenar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Poisoned}}
  , PokemonMov {attackName = "Paralizar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Hierba, movParams = StatusMov {statusType = Paralized}}]
  },
  Pokemon {
    name = "Snorlax"
  , pokedexNumber = 3
  , stats = PokemonStatistics {pokemonType = [Normal], maxPs = 180, currentPs = 180, attack = 45, defense = 50, spAttack = 10, spDefense = 50, speed = 10, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Hiperrayo", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 100, dmgType = Physic}}
  , PokemonMov {attackName = "Panzazo", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = DmgMov {power = 65, dmgType = Physic}}
  , PokemonMov {attackName = "Paz Mental", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Defense, SpecialDefense]}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}]
  },
  Pokemon {
    name = "Charizard"
  , pokedexNumber = 6
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 85, defense = 20, spAttack = 50, spDefense = 20, speed = 25, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 75, dmgType = Physic}}
  , PokemonMov {attackName = "Aceleracion", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 2.0, upgradedStats = [Speed]}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  }

  Pokemon {
    name = "Arcanine"
  , pokedexNumber = 18
  , stats = PokemonStatistics {pokemonType = [Fuego], maxPs = 100, currentPs = 100, attack = 45, defense = 15, spAttack = 25, spDefense = 15, speed = 40, crit = 0.05}
  , status = Nothing
  , movs = S.fromList [
    PokemonMov {attackName = "Bola Fuego", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 35, dmgType = Physic}}
  , PokemonMov {attackName = "Lanzallamas", movType = Dmg, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = DmgMov {power = 55, dmgType = Special}}
  , PokemonMov {attackName = "Danza Espada", movType = Buff, movsLeft = 10, accuracy = 1.0, pokType = Normal, movParams = BuffMov {multiplier = 1.5, upgradedStats = [Attack, SpecialAttack]}}
  , PokemonMov {attackName = "Quemar", movType = Status, movsLeft = 10, accuracy = 1.0, pokType = Fuego, movParams = StatusMov {statusType = Burned}}]
  },
assets/ashImages/1.bmp
assets/ashImages/2.bmp
assets/ashImages/3.bmp
assets/ashImages/4.bmp
assets/ashImages/5.bmp
assets/ashImages/6.bmp
assets/ashImages/7.bmp
assets/ashImages/8.bmp
assets/ashImages/9.bmp
assets/ashImages/37.bmp
assets/ashImages/38.bmp
assets/ashImages/58.bmp
assets/ashImages/59.bmp
assets/ashImages/60.bmp
assets/ashImages/61.bmp
assets/ashImages/62.bmp
assets/ashImages/69.bmp
assets/ashImages/70.bmp
assets/ashImages/71.bmp
assets/ashImages/72.bmp
assets/ashImages/73.bmp
assets/ashImages/77.bmp
assets/ashImages/78.bmp
assets/ashImages/131.bmp
assets/ashImages/143.bmp

  assets/garyImages/10.bmp
assets/garyImages/11.bmp
assets/garyImages/12.bmp
assets/garyImages/13.bmp
assets/garyImages/14.bmp
assets/garyImages/15.bmp
assets/garyImages/16.bmp
assets/garyImages/17.bmp

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