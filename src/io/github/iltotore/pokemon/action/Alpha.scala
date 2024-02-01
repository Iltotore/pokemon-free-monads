package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.*

enum Alpha[+Out]:
  case GetHealth(pokemon: WhichPokemon) extends Alpha[Double]
  case GetMaxHealth(pokemon: WhichPokemon) extends Alpha[Double]
  case GetType(pokemon: WhichPokemon) extends Alpha[Type]
  case GetStatus(pokemon: WhichPokemon) extends Alpha[Status]
  case SetHealth(pokemon: WhichPokemon, health: Double) extends Alpha[Unit]
  case SetType(pokemon: WhichPokemon, tpe: Type) extends Alpha[Unit]
  case SetStatus(pokemon: WhichPokemon, status: Status) extends Alpha[Unit]
  case SetActivePokemon(player: WhichPlayer, slot: Int) extends Alpha[Unit]
  case Random(chance: Double, program: Alpha[Unit]) extends Alpha[Unit]
  case FlatMap[A, B](program: Alpha[A], f: A => Alpha[B]) extends Alpha[B]
  case Pure(result: Out)

  def flatMap[B](f: Out => Alpha[B]): Alpha[B] = FlatMap(this, f)

  def map[B](f: Out => B): Alpha[B] = flatMap(out => Pure(f(out)))

  def evaluate(game: Game): (Game, Out) = this match
    case GetHealth(pokemon)             => (game, game.getPokemon(pokemon).currentHealth)
    case GetMaxHealth(pokemon)          => (game, game.getPokemon(pokemon).maxHealth)
    case GetType(pokemon)               => (game, game.getPokemon(pokemon).pokemonType)
    case GetStatus(pokemon)             => (game, game.getPokemon(pokemon).status)
    case SetHealth(pokemon, health)     => (game.modifyPokemon(pokemon, _.copy(currentHealth = health)), ())
    case SetType(pokemon, tpe)          => (game.modifyPokemon(pokemon, _.copy(pokemonType = tpe)), ())
    case SetStatus(pokemon, status)     => (game.modifyPokemon(pokemon, _.copy(status = status)), ())
    case SetActivePokemon(player, slot) => (game.modifyPlayer(player, _.copy(activeSlot = slot)), ())
    case Random(chance, program) =>
      if math.random() <= chance then program.evaluate(game)
      else (game, ())

    case FlatMap(program, f) =>
      val (gameState, result) = program.evaluate(game)
      f(result).evaluate(gameState)

    case Pure(result) => (game, result)

object Alpha:

  def NoAction: Alpha[Unit] = Pure(())
