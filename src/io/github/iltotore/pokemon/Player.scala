package io.github.iltotore.pokemon

case class Player(name: String, activeSlot: Int, team: List[Pokemon]):

  def alive: Boolean = team.exists(_.alive)

  def activePokemon: Pokemon = team(activeSlot)

  def withTeamMember(slot: Int, pokemon: Pokemon): Player = this.copy(team = team.updated(slot, pokemon))

  def modifyTeamMember(slot: Int, f: Pokemon => Pokemon): Player = withTeamMember(slot, f(team(slot)))

  def withActivePokemon(pokemon: Pokemon): Player = withTeamMember(activeSlot, pokemon)

  def modifyActivePokemon(f: Pokemon => Pokemon): Player = withActivePokemon(f(activePokemon))
