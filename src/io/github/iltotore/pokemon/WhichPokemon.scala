package io.github.iltotore.pokemon

enum WhichPokemon:
  case Active(player: WhichPlayer)
  case InTeam(player: WhichPlayer, slot: Int)

  def player: WhichPlayer

  def active: Boolean = this match
    case Active(player)       => true
    case InTeam(player, slot) => false
