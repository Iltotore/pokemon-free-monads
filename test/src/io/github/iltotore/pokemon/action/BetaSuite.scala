package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.*
import io.github.iltotore.pokemon.WhichPlayer.*
import io.github.iltotore.pokemon.WhichPokemon.*
import io.github.iltotore.pokemon.ability.Ability
import io.github.iltotore.pokemon.action.Beta.*
import io.github.iltotore.pokemon.util.Free
import utest.*

object BetaSuite extends TestSuite:

  val tests = Tests:

    val game = Game(
      turn = 0,
      players = Map(
        WhichPlayer.PlayerA -> Player(
          name = "Il_totore",
          activeSlot = 0,
          List(
            Pokemon(
              species = "Evee",
              pokemonType = Type.Normal,
              speed = 10,
              currentHealth = 75,
              maxHealth = 100,
              status = Status.Healthy,
              ability = Ability.NoAbility,
              moves = List.empty
            ),
            Pokemon(
              species = "Ghastly",
              pokemonType = Type.Ghost,
              speed = 10,
              currentHealth = 100,
              maxHealth = 100,
              status = Status.Burn,
              ability = Ability.NoAbility,
              moves = List.empty
            )
          )
        )
      )
    )

    def assertEffect[Out](program: Beta[Out])(expectedState: Game, expectedResult: Out): Unit =
      val result =
        program
          .foldMap(Beta.toAlpha)
          .foldMap(Alpha.toGameEffect)
          .run(game)

      assert(result == (expectedState, expectedResult))

    def assertResult[Out](program: Beta[Out])(expectedResult: Out): Unit =
      assertEffect(program)(game, expectedResult)

    def assertState(program: Beta[Unit])(expectedState: Game): Unit =
      assertEffect(program)(expectedState, ())

    test("evaluate"):

      val evee = Active(PlayerA)
      val ghastly = InTeam(PlayerA, 1)

      test("getHealth") - assertResult(getHealth(evee))(75)
      test("getMaxHealth") - assertResult(getMaxHealth(evee))(100)

      test("damage"):
        test("<100") - assertState(damage(evee, Cause.Self, 50))(game.modifyPokemon(evee, _.copy(currentHealth = 25)))
        test(">100") - assertState(damage(evee, Cause.Self, 200))(game.modifyPokemon(evee, _.copy(currentHealth = 0)))

      test("heal"):
        test("<100") - assertState(heal(evee, Cause.Self, 20))(game.modifyPokemon(evee, _.copy(currentHealth = 95)))
        test(">100") - assertState(heal(evee, Cause.Self, 200))(game.modifyPokemon(evee, _.copy(currentHealth = 100)))

      test("switchIn") - assertState(switchIn(PlayerA, 1))(game.modifyPlayer(PlayerA, _.copy(activeSlot = 1)))

      test("inflictStatus"):
        test("onHealthy") - assertState(inflictStatus(evee, Status.Poison))(game.modifyPokemon(evee, _.copy(status = Status.Poison)))
        test("alreadyStatused") - assertState(inflictStatus(ghastly, Status.Poison))(game)

      test("cureStatus") - assertState(cureStatus(ghastly))(game.modifyPokemon(ghastly, _.copy(status = Status.Healthy)))

    test("rewrite"):

      val pokemon = Active(PlayerA)

      def assertEquivalent[Out](programA: Beta[Out], programB: Beta[Out]): Unit =
        val resultA =
          programA
            .foldMap(Beta.toAlpha)
            .foldMap(Alpha.toGameEffect)
            .run(game)

        val resultB =
          programB
            .foldMap(Beta.toAlpha)
            .foldMap(Alpha.toGameEffect)
            .run(game)

        assert(resultA == resultB)

      def assertRewrite[Out, A](program: Beta[Out], expected: Beta[Out])(f: PartialFunction[Beta.Algebra[A], Beta[A]]): Unit =
        assertEquivalent(program.rewrite(f), expected)

      test("leaf"):
        assertRewrite[Unit, Unit](
          program = damage(pokemon, Cause.Self, 20),
          expected = heal(pokemon, Cause.Self, 20)
        ):
          case Algebra.Damage(pokemon, cause, amount) => heal(pokemon, cause, amount)

      test("noMatch"):
        assertRewrite[Unit, Unit](
          program = damage(pokemon, Cause.Self, 20),
          expected = damage(pokemon, Cause.Self, 20)
        ):
          case Algebra.InflictStatus(pokemon, Status.Poison) => cureStatus(pokemon)

      test("flatMap"):
        val program =
          for
            maxHealth <- getMaxHealth(pokemon)
            _ <- damage(pokemon, Cause.StatusEffect(Status.Poison), maxHealth * 0.12)
          yield ()

        val expected =
          for
            maxHealth <- getMaxHealth(pokemon)
            _ <- heal(pokemon, Cause.StatusEffect(Status.Poison), maxHealth * 0.12)
          yield ()

        assertRewrite[Unit, Unit](program, expected):
          case Algebra.Damage(pokemon, Cause.StatusEffect(Status.Poison), amount) =>
            heal(pokemon, Cause.StatusEffect(Status.Poison), amount)