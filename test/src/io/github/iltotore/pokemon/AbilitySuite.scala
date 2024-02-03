package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.WhichPokemon.*
import io.github.iltotore.pokemon.WhichPlayer.*
import io.github.iltotore.pokemon.ability.Ability
import io.github.iltotore.pokemon.action.{Alpha, Beta, Cause}
import io.github.iltotore.pokemon.action.Beta.*
import utest.*

object AbilitySuite extends TestSuite:

  val tests = Tests:

    val game = Game(
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
              moves = List.empty
            ),

            Pokemon(
              species = "Jolteon",
              pokemonType = Type.Electric,
              speed = 10,
              currentHealth = 100,
              maxHealth = 100,
              status = Status.Burn,
              ability = Ability.Guts,
              moves = List.empty
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
              moves = List.empty
            ),
            Pokemon(
              species = "Breloom",
              pokemonType = Type.Grass,
              speed = 10,
              currentHealth = 88,
              maxHealth = 100,
              status = Status.Poison,
              ability = Ability.PoisonHeal,
              moves = List.empty
            )
          )
        )
      )
    )

    val snivy = Active(PlayerA)
    val slowbro = Active(PlayerB)
    val jolteon = InTeam(PlayerA, 1)
    val breloom = InTeam(PlayerB, 1)

    def assertState(owner: WhichPokemon, ability: Ability, program: Beta[Unit])(expectedState: Game): Unit =
      val rewritten = ability.effect(owner, program)
      val compiled = rewritten.foldMap(Beta.toAlpha)
      val result = compiled.foldMap(Alpha.toGameEffect).run(game)._1

      assert(result == expectedState)

    test("overgrow") - assertState(
      owner = snivy,
      ability = Ability.Overgrow,
      program = damage(breloom, Cause.Attack(snivy, Type.Grass), 50)
    )(game.modifyPokemon(breloom, _.copy(currentHealth = 88 - (50 * 1.5 / 2))))

    test("poisonHeal") - assertState(
      owner = breloom,
      ability = Ability.PoisonHeal,
      program = damage(breloom, Cause.StatusEffect(Status.Poison), 12)
    )(game.modifyPokemon(breloom, _.copy(currentHealth = 100)))

    test("guts") - assertState(
      owner = jolteon,
      ability = Ability.Guts,
      program = damage(breloom, Cause.Attack(jolteon, Type.Electric), 10)
    )(game.modifyPokemon(breloom, _.copy(currentHealth = 88 - 15)))

    test("regenerator") - assertState(
      owner = slowbro,
      ability = Ability.Regenerator,
      program = switchIn(PlayerB, 1)
    )(
      game
        .modifyPokemon(slowbro, _.copy(currentHealth = 100))
        .modifyPlayer(PlayerB, _.copy(activeSlot = 1))
    )