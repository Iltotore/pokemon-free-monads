package io.github.iltotore.pokemon.ability

import io.github.iltotore.pokemon.action.Beta.*
import io.github.iltotore.pokemon.action.{Beta, Cause}
import io.github.iltotore.pokemon.{Status, Type}

case class Ability(name: String, effect: AbilityEffect)

object Ability:

  val NoAbility: Ability = Ability(
    name = "Nothing",
    effect = (owner, program) => program
  )

  val Overgrow: Ability = Ability(
    name = "Overgrow",
    effect = (owner, program) =>
      program.rewrite:
        case Algebra.Damage(pokemon, Cause.Attack(attacker, Type.Grass), amount) if attacker == owner =>
          for
            health <- getHealth(owner)
            maxHealth <- getMaxHealth(owner)
            dmg =
              if health <= maxHealth / 3 then amount * 1.5
              else amount

            _ <- damage(pokemon, Cause.Attack(owner, Type.Grass), dmg)
          yield ()
  )

  val PoisonHeal: Ability = Ability(
    name = "Poison Heal",
    effect = (owner, program) =>
      program.rewrite[Unit]:
        case Algebra.Damage(pokemon, Cause.StatusEffect(Status.Poison), _) if pokemon == owner =>
          healPercent(pokemon, Cause.StatusEffect(Status.Poison), 0.125)
  )

  val Guts: Ability = Ability(
    name = "Guts",
    effect = (owner, program) =>
      program.rewrite:
        case Algebra.Damage(pokemon, Cause.Attack(attacker, tpe), amount) if attacker == owner =>
          for
            status <- getStatus(owner)
            dmg =
              if status == Status.Healthy then amount
              else amount * 1.5

            _ <- damage(pokemon, Cause.Attack(owner, tpe), dmg)
          yield ()
  )

  val Regenerator: Ability = Ability(
    name = "Regenerator",
    effect = (owner, program) =>
      program.rewrite:
        case Algebra.SwitchIn(player, slot) if owner.active && owner.player == player =>
          for
            _ <- healPercent(owner, Cause.Self, 0.33)
            _ <- switchIn(player, slot)
          yield ()
  )
