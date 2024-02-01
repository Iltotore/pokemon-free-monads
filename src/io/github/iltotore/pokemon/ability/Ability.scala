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
        case Damage(pokemon, Cause.Attack(attacker, Type.Grass), amount) if attacker == owner =>
          for
            health <- GetHealth(owner)
            maxHealth <- GetMaxHealth(owner)
            damage =
              if health <= maxHealth / 3 then amount * 1.5
              else amount

            _ <- Damage(pokemon, Cause.Attack(owner, Type.Grass), damage)
          yield ()
  )

  val PoisonHeal: Ability = Ability(
    name = "Poison Heal",
    effect = (owner, program) =>
      program.rewrite:
        case Damage(pokemon, Cause.StatusEffect(Status.Poison), _) if pokemon == owner =>
          for
            maxHealth <- GetMaxHealth(owner)
            _ <- Heal(owner, Cause.StatusEffect(Status.Poison), maxHealth * 0.12)
          yield ()
  )

  val Guts: Ability = Ability(
    name = "Guts",
    effect = (owner, program) =>
      program.rewrite:
        case Damage(pokemon, Cause.Attack(attacker, tpe), amount) if attacker == owner =>
          for
            status <- GetStatus(owner)
            damage =
              if status == Status.Healthy then amount
              else amount * 1.5

            _ <- Damage(pokemon, Cause.Attack(owner, tpe), damage)
          yield ()
  )

  val Regenerator: Ability = Ability(
    name = "Regenerator",
    effect = (owner, program) =>
      program.rewrite:
        case SwitchIn(player, slot) if owner.active && owner.player == player =>
          for
            maxHealth <- GetMaxHealth(owner)
            _ <- Heal(owner, Cause.Self, maxHealth * 0.33)
            _ <- SwitchIn(player, slot)
          yield ()
  )
