package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.action.*

import scala.util.Random

case class Game(turn: Int, players: Map[WhichPlayer, Player], random: Random = Random()):

  def result: Option[GameResult] =
    val alive = players.collect:
      case (which, player) if player.alive => player

    if alive.size >= 2 then None
    else Some(alive.headOption.fold(GameResult.Tie)(GameResult.Win(_)))

  def modifyPlayer(which: WhichPlayer, f: Player => Player): Game =
    this.copy(players = players.updatedWith(which)(_.map(f)))

  def getPokemon(which: WhichPokemon): Pokemon = which match
    case WhichPokemon.Active(player)       => players(player).activePokemon
    case WhichPokemon.InTeam(player, slot) => players(player).team(slot)

  def withPokemon(which: WhichPokemon, pokemon: Pokemon): Game = which match
    case WhichPokemon.Active(player)       => modifyPlayer(player, _.withActivePokemon(pokemon))
    case WhichPokemon.InTeam(player, slot) => modifyPlayer(player, _.withTeamMember(slot, pokemon))

  def modifyPokemon(which: WhichPokemon, f: Pokemon => Pokemon): Game =
    withPokemon(which, f(getPokemon(which)))

  def doTurn(actions: List[(WhichPlayer, Gamma)]): Game =
    val ordered = actions.sortWith((a, b) => a._2.compare(this, b._2) < 0)

    val afterTurn = ordered.foldLeft(this):
      case (game, (player, theta)) =>
        val playerActive = WhichPokemon.Active(player)
        val playerActivePokemon = game.getPokemon(playerActive)

        if playerActivePokemon.alive then
          val activeAbilities = game.players.map(_._2.activePokemon.ability)

          val thetaStatus = playerActivePokemon.status.effectTheta(theta)
          val compiled = activeAbilities.foldLeft(thetaStatus.toBeta)((action, ability) => ability.effect(playerActive, action))
          val beta = playerActivePokemon.status.effectBeta(playerActive, compiled)

          beta
            .foldMap(Beta.toAlpha)
            .foldMap(Alpha.toGameEffect)
            .run(game)
            ._1
        else game

    val endOfTurn = players.foldLeft(afterTurn):
      case (game, (which, player)) =>
        val playerActive = WhichPokemon.Active(which)
        val playerActivePokemon = game.getPokemon(playerActive)

        if playerActivePokemon.alive then
          val activeAbilities = game.players.map(_._2.activePokemon.ability)

          val endOfTurnStatus = playerActivePokemon.status.effectEndTurn(playerActive)
          val compiled = activeAbilities.foldLeft(endOfTurnStatus)((effect, ability) => ability.effect(playerActive, effect))
          val beta = playerActivePokemon.status.effectBeta(playerActive, compiled)

          beta
            .foldMap(Beta.toAlpha)
            .foldMap(Alpha.toGameEffect)
            .run(game)
            ._1
        else game

    endOfTurn.copy(turn = endOfTurn.turn + 1)
