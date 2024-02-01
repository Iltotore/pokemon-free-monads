package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.{Status, WhichPlayer, WhichPokemon}

enum Beta[+Out]:
  case GetHealth(pokemon: WhichPokemon) extends Beta[Double]
  case GetMaxHealth(pokemon: WhichPokemon) extends Beta[Double]
  case GetStatus(pokemon: WhichPokemon) extends Beta[Status]
  case SetStatus(pokemon: WhichPokemon, status: Status) extends Beta[Unit]
  case Damage(pokemon: WhichPokemon, cause: Cause, amount: Double) extends Beta[Unit]
  case Heal(pokemon: WhichPokemon, cause: Cause, amount: Double) extends Beta[Unit]
  case SwitchIn(player: WhichPlayer, slot: Int) extends Beta[Unit]
  case InflictStatus(pokemon: WhichPokemon, status: Status) extends Beta[Unit]
  case CureStatus(pokemon: WhichPokemon) extends Beta[Unit]
  case Random(chance: Double, program: Beta[Unit]) extends Beta[Unit]
  case FlatMap[A, B](program: Beta[A], f: A => Beta[B]) extends Beta[B]
  case Pure(result: Out)

  def flatMap[B](f: Out => Beta[B]): Beta[B] = FlatMap(this, f)

  def map[B](f: Out => B): Beta[B] = flatMap(out => Pure(f(out)))

  def rewrite[A](rewriteRule: PartialFunction[Beta[A], Beta[A]]): Beta[Out] = this match
    // Despite the "unchecked" warning, `isDefinedAt` checks if the rule accepts the program so this case is sound.
    case program: Beta[A] @unchecked if rewriteRule.isDefinedAt(program) => rewriteRule(program).asInstanceOf[Beta[Out]]
    case FlatMap(program, f)                                             => FlatMap(program.rewrite(rewriteRule), p => f(p).rewrite(rewriteRule))
    case program                                                         => program

  def compile: Alpha[Out] = this match
    case GetHealth(pokemon)         => Alpha.GetHealth(pokemon)
    case GetMaxHealth(pokemon)      => Alpha.GetMaxHealth(pokemon)
    case GetStatus(pokemon)         => Alpha.GetStatus(pokemon)
    case SetStatus(pokemon, status) => Alpha.SetStatus(pokemon, status)

    case Damage(pokemon, cause, amount) =>
      for
        health <- Alpha.GetHealth(pokemon)
        effectiveness <- cause match
          case Cause.Attack(_, tpe) =>
            for
              defTpe <- Alpha.GetType(pokemon)
            yield defTpe.effectiveness(tpe).coefficient

          case _ => Alpha.Pure(1.0)

        _ <- Alpha.SetHealth(pokemon, math.max(0, health - amount * effectiveness))
        h <- Alpha.GetHealth(pokemon)
      yield ()

    case Heal(pokemon, cause, amount) =>
      for
        health <- Alpha.GetHealth(pokemon)
        maxHealth <- Alpha.GetMaxHealth(pokemon)
        _ <- Alpha.SetHealth(pokemon, math.min(maxHealth, health + amount))
      yield ()

    case SwitchIn(player, slot) => Alpha.SetActivePokemon(player, slot)
    case InflictStatus(pokemon, status) =>
      for
        current <- Alpha.GetStatus(pokemon)
        _ <-
          if current == Status.Healthy && status != Status.Healthy then Alpha.SetStatus(pokemon, status)
          else Alpha.Pure(())
      yield ()

    case CureStatus(pokemon)     => Alpha.SetStatus(pokemon, Status.Healthy)
    case Random(chance, program) => Alpha.Random(chance, program.compile)
    case FlatMap(program, f)     => Alpha.FlatMap(program.compile, f.andThen(_.compile))
    case Pure(value)             => Alpha.Pure(value)

object Beta:

  def damagePercent(pokemon: WhichPokemon, cause: Cause, percent: Double): Beta[Unit] =
    for
      maxHealth <- GetMaxHealth(pokemon)
      _ <- Damage(pokemon, cause, percent * maxHealth)
    yield ()

  def nothing: Beta[Unit] = Pure(())
