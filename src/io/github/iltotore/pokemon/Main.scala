package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.ability.Ability
import io.github.iltotore.pokemon.move.Move

object Main:

  @main def mainMethod =
    var game = Game(
      turn = 0,
      players = Map(
        WhichPlayer.PlayerA -> Player(
          name = "Il_totore",
          activeSlot = 0,
          List(
            Pokemon(
              species = "Snivy",
              pokemonType = Type.Grass,
              speed = 10,
              currentHealth = 25,
              maxHealth = 100,
              status = Status.Healthy,
              ability = Ability.Overgrow,
              moves = List(Move.EnergyBall, Move.Toxic, Move.Recover)
            ),
            Pokemon(
              species = "Jolteon",
              pokemonType = Type.Electric,
              speed = 10,
              currentHealth = 100,
              maxHealth = 100,
              status = Status.Burn,
              ability = Ability.Guts,
              moves = List(Move.EnergyBall)
            )
          )
        ),
        WhichPlayer.PlayerB -> Player(
          name = "Aless",
          activeSlot = 0,
          List(
            Pokemon(
              species = "Slowbro",
              pokemonType = Type.Water,
              speed = 10,
              currentHealth = 77,
              maxHealth = 100,
              status = Status.Healthy,
              ability = Ability.Regenerator,
              moves = List(Move.HydroPump, Move.Toxic, Move.Recover)
            ),
            Pokemon(
              species = "Breloom",
              pokemonType = Type.Grass,
              speed = 10,
              currentHealth = 88,
              maxHealth = 100,
              status = Status.Poison,
              ability = Ability.PoisonHeal,
              moves = List(Move.EnergyBall, Move.Toxic)
            )
          )
        )
      )
    )

    while game.result.isEmpty do
      println(tui.showState(game))
      val switches =
        for
          (which, player) <- game.players
          if player.alive && !player.activePokemon.alive
        yield tui.ask(s"${player.name}, choose the pokemon to send", tui.parseSwitch(which, player, _))

      game = switches.foldLeft(game)((g, program) => program.run(game))
      if switches.nonEmpty then println(tui.showState(game))

      val actions =
        for
          (which, player) <- game.players
          if player.alive
        yield (which, tui.ask(s"What should ${player.activePokemon.species} do?", tui.parseAction(which, game, _)))

      game = game.doTurn(actions.toList)

    println(game.result.get)
