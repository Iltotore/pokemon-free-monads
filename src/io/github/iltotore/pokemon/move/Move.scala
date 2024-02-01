package io.github.iltotore.pokemon.move

import io.github.iltotore.pokemon.{Status, Type}

case class Move(name: String, tpe: Type, effect: MoveEffect)

object Move:
  val EnergyBall: Move = Move("Energy Ball", Type.Grass, MoveEffect.attack(Type.Grass, 90))
  val HydroPump: Move = Move("Hydro Pump", Type.Water, MoveEffect.attack(Type.Water, 110))
  val Toxic: Move = Move("Toxic", Type.Normal, MoveEffect.inflict(Status.Poison))
  val Recover: Move = Move("Recover", Type.Normal, MoveEffect.healPercent(0.5))
