package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.{Status, Type, WhichPokemon}

enum Cause:
  case Self
  case StatusEffect(status: Status)
  case Attack(pokemon: WhichPokemon, tpe: Type)
