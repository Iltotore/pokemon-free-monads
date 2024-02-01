package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.WhichPokemon.*
import io.github.iltotore.pokemon.WhichPlayer.*
import io.github.iltotore.pokemon.ability.Ability
import io.github.iltotore.pokemon.action.Beta
import io.github.iltotore.pokemon.move.MoveEffect
import io.github.iltotore.pokemon.move.Move.*
import utest.*

object MoveSuite extends TestSuite:
  
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
              currentHealth = 100,
              maxHealth = 100,
              status = Status.Healthy,
              ability = Ability.Overgrow,
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
              currentHealth = 200,
              maxHealth = 300,
              status = Status.Healthy,
              ability = Ability.Regenerator,
              moves = List.empty
            )
          )
        )
      )
    )

    def assertState(effect: MoveEffect, user: WhichPokemon, target: WhichPokemon)(expectedState: Game): Unit =
      val result = effect(user, target).compile.evaluate(game)._1

      assert(result == expectedState)
      
    val snivy = Active(PlayerA)
    val slowbro = Active(PlayerB)
    
    test("energyBall") - assertState(EnergyBall.effect, snivy, slowbro)(game.modifyPokemon(slowbro, _.copy(currentHealth = 20)))
    test("hydroPump") - assertState(HydroPump.effect, slowbro, snivy)(game.modifyPokemon(snivy, _.copy(currentHealth = 45)))
    test("toxic") - assertState(Toxic.effect, snivy, slowbro)(game.modifyPokemon(slowbro, _.copy(status = Status.Poison)))
    test("recover") - assertState(Recover.effect, slowbro, snivy)(game.modifyPokemon(slowbro, _.copy(currentHealth = 300)))

