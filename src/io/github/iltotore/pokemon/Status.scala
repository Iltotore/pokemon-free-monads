package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.action.{Beta, Cause, Theta}

enum Status:
  case Healthy
  case Burn
  case Paralysis
  case Poison
  case Sleep(turnsLeft: Int)

  def name: String = this match
    case Healthy   => "Healthy"
    case Burn      => "Burn"
    case Paralysis => "Paralysis"
    case Poison    => "Poison"
    case Sleep(_)  => "Sleep"

  def effectBeta(pokemon: WhichPokemon, program: Beta[?]): Beta[?] = this match
    case Healthy => program
    case Burn => program.rewrite:
        case Beta.Algebra.Damage(target, Cause.Attack(user, tpe), amount) if user == pokemon =>
          Beta.damage(target, Cause.Attack(user, tpe), amount / 2)

    case Paralysis => program
    case Poison    => program
    case Sleep(_)  => program

  def effectTheta(program: Theta): Theta = this match
    case Healthy => program
    case Burn    => program
    case Paralysis =>
      program match
        case _: Theta.UseMove => Theta.Random(0.33, program)
        case _                => program

    case Poison => program
    case Sleep(turnsLeft) if turnsLeft > 0 =>
      Theta.Cancel(program, Beta.setStatus(program.affectedPokemon, Sleep(turnsLeft - 1)))
    case Sleep(_) => Theta.Before(program, Beta.cureStatus(program.affectedPokemon))

  def effectEndTurn(pokemon: WhichPokemon): Beta[Unit] = this match
    case Healthy          => Beta.unit
    case Burn             => Beta.damagePercent(pokemon, Cause.StatusEffect(Burn), 0.06)
    case Paralysis        => Beta.unit
    case Poison           => Beta.damagePercent(pokemon, Cause.StatusEffect(Poison), 0.12)
    case Sleep(turnsLeft) => Beta.unit
