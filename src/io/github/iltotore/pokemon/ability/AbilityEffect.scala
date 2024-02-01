package io.github.iltotore.pokemon.ability

import io.github.iltotore.pokemon.WhichPokemon
import io.github.iltotore.pokemon.action.Beta

trait AbilityEffect:

  def apply(owner: WhichPokemon, program: Beta[Unit]): Beta[Unit]
