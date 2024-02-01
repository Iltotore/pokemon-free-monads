package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.ability.Ability
import io.github.iltotore.pokemon.move.Move

case class Pokemon(
    species: String,
    pokemonType: Type,
    speed: Int,
    currentHealth: Double,
    maxHealth: Double,
    status: Status,
    ability: Ability,
    moves: List[Move]
):

  def alive: Boolean = currentHealth > 0
