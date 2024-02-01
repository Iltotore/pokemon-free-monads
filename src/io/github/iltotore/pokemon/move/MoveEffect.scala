package io.github.iltotore.pokemon.move

import io.github.iltotore.pokemon.action.Beta.*
import io.github.iltotore.pokemon.action.{Beta, Cause}
import io.github.iltotore.pokemon.{Status, Type, WhichPokemon}

trait MoveEffect:

  def apply(user: WhichPokemon, target: WhichPokemon): Beta[Unit]

object MoveEffect:

  def attack(tpe: Type, amount: Double): MoveEffect = (user, target) => Damage(target, Cause.Attack(user, tpe), amount)

  def healPercent(percent: Double): MoveEffect = (user, target) =>
    for
      maxHealth <- GetMaxHealth(user)
      _ <- Heal(user, Cause.Self, maxHealth * percent)
    yield ()

  def inflict(status: Status): MoveEffect = (user, target) => InflictStatus(target, status)
