package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.*
import io.github.iltotore.pokemon.WhichPokemon.*
import io.github.iltotore.pokemon.WhichPlayer.*
import io.github.iltotore.pokemon.ability.Ability
import io.github.iltotore.pokemon.action.Alpha.*
import utest.*

object AlphaSuite extends TestSuite:

  val tests = Tests:

    val game = Game(
      turn = 0,
      players = Map(
        WhichPlayer.PlayerA -> Player(
          name = "Il_totore",
          activeSlot = 0,
          List(Pokemon(
            species = "Evee",
            pokemonType = Type.Normal,
            speed = 10,
            currentHealth = 100,
            maxHealth = 100,
            status = Status.Healthy,
            ability = Ability.NoAbility,
            moves = List.empty
          ))
        )
      )
    )

    def assertEffect[Out](program: Alpha[Out])(expectedState: Game, expectedResult: Out): Unit =
      assert(program.foldMap(Alpha.toGameEffect).run(game) == (expectedState, expectedResult))

    def assertResult[Out](program: Alpha[Out])(expectedResult: Out): Unit =
      assertEffect(program)(game, expectedResult)

    def assertState(program: Alpha[Unit])(expectedState: Game): Unit =
      assertEffect(program)(expectedState, ())

    test("getHealth") - assertResult(getHealth(Active(PlayerA)))(100)
    test("getMaxHealth") - assertResult(getMaxHealth(Active(PlayerA)))(100)
    test("getType") - assertResult(getType(Active(PlayerA)))(Type.Normal)
    test("getStatus") - assertResult(getStatus(Active(PlayerA)))(Status.Healthy)

    test("setHealth") - assertState(
      setHealth(Active(PlayerA), 50)
    )(game.modifyPokemon(Active(PlayerA), _.copy(currentHealth = 50)))

    test("setType") - assertState(
      setType(Active(PlayerA), Type.Fire)
    )(game.modifyPokemon(Active(PlayerA), _.copy(pokemonType = Type.Fire)))

    test("setStatus") - assertState(
      setStatus(Active(PlayerA), Status.Poison)
    )(game.modifyPokemon(Active(PlayerA), _.copy(status = Status.Poison)))

    test("setCurrentPokemon") - assertState(
      setActivePokemon(PlayerA, 1)
    )(game.modifyPlayer(PlayerA, _.copy(activeSlot = 1)))

    test("flatMap"):

      val program =
        for
          health <- getHealth(Active(PlayerA))
          _      <- setHealth(Active(PlayerA), health/2)
        yield
          ()

      assertState(program)(game.modifyPokemon(Active(PlayerA), _.copy(currentHealth = 50)))

    test("pure") - assertResult(pure(42))(42)