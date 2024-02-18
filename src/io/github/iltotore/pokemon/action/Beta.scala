package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.util.*
import io.github.iltotore.pokemon.{Status, WhichPlayer, WhichPokemon}

type Beta[A] = Free[Beta.Algebra, A]
object Beta:

  enum Algebra[Out]:
    case GetHealth(pokemon: WhichPokemon) extends Algebra[Double]
    case GetMaxHealth(pokemon: WhichPokemon) extends Algebra[Double]
    case GetStatus(pokemon: WhichPokemon) extends Algebra[Status]
    case Random() extends Algebra[Double]
    case Damage(pokemon: WhichPokemon, cause: Cause, amount: Double) extends Algebra[Unit]
    case Heal(pokemon: WhichPokemon, cause: Cause, amount: Double) extends Algebra[Unit]
    case SwitchIn(player: WhichPlayer, slot: Int) extends Algebra[Unit]
    case SetStatus(pokemon: WhichPokemon, status: Status) extends Algebra[Unit]
    case InflictStatus(pokemon: WhichPokemon, status: Status) extends Algebra[Unit]
    case CureStatus(pokemon: WhichPokemon) extends Algebra[Unit]

  import Algebra.*
  
  val unit: Beta[Unit] = Free.pure(())

  def getHealth(pokemon: WhichPokemon): Beta[Double] = Free.liftM(GetHealth(pokemon))

  def getMaxHealth(pokemon: WhichPokemon): Beta[Double] = Free.liftM(GetMaxHealth(pokemon))

  def getStatus(pokemon: WhichPokemon): Beta[Status] = Free.liftM(GetStatus(pokemon))
  
  def random(): Beta[Double] = Free.liftM(Random())
  
  def randomInt(max: Int): Beta[Int] = Free.liftM(Random()).map(x => (x * max).toInt)
  
  def chooseRandom(chance: Double, thenDo: Beta[Unit], orElse: Beta[Unit] = Beta.unit): Beta[Unit] =
    random().flatMap: x =>
      if x <= chance then thenDo
      else orElse
  
  def damage(pokemon: WhichPokemon, cause: Cause, amount: Double): Beta[Unit] = Free.liftM(Damage(pokemon, cause, amount))
  
  def damagePercent(pokemon: WhichPokemon, cause: Cause, percent: Double): Beta[Unit] =
    for
      maxHealth <- getMaxHealth(pokemon)
      _         <- damage(pokemon, cause, maxHealth * percent)
    yield ()

  def heal(pokemon: WhichPokemon, cause: Cause, amount: Double): Beta[Unit] = Free.liftM(Heal(pokemon, cause, amount))

  def healPercent(pokemon: WhichPokemon, cause: Cause, percent: Double): Beta[Unit] =
    for
      maxHealth <- getMaxHealth(pokemon)
      _ <- heal(pokemon, cause, maxHealth * percent)
    yield ()

  def switchIn(player: WhichPlayer, slot: Int): Beta[Unit] = Free.liftM(SwitchIn(player, slot))

  def setStatus(pokemon: WhichPokemon, status: Status): Beta[Unit] = Free.liftM(SetStatus(pokemon, status))
  
  def inflictStatus(pokemon: WhichPokemon, status: Status): Beta[Unit] = Free.liftM(InflictStatus(pokemon, status))

  def cureStatus(pokemon: WhichPokemon): Beta[Unit] = Free.liftM(CureStatus(pokemon))
  
  def toAlpha: Algebra ~> Alpha = new:

    override def apply[A](m: Algebra[A]): Alpha[A] = m match
      case GetHealth(pokemon)    => Alpha.getHealth(pokemon)
      case GetMaxHealth(pokemon) => Alpha.getMaxHealth(pokemon)
      case GetStatus(pokemon)    => Alpha.getStatus(pokemon)
      case Random()              => Alpha.random()
      case Damage(pokemon, cause, amount) =>
        for
          effectiveness <- cause match
            case Cause.Attack(_, tpe) =>
              for
                defTpe <- Alpha.getType(pokemon)
              yield defTpe.effectiveness(tpe).coefficient

            case _ => Alpha.pure(1.0)

          _ <- Alpha.decreaseHealth(pokemon, amount * effectiveness)
        yield ()

      case Heal(pokemon, cause, amount)   => Alpha.increaseHealth(pokemon, amount)
      case SwitchIn(player, slot)         => Alpha.setActivePokemon(player, slot)
      case SetStatus(pokemon, status)     => Alpha.setStatus(pokemon, status)
      case InflictStatus(pokemon, status) =>
        for
          current <- Alpha.getStatus(pokemon)
          _ <-
            if current == Status.Healthy && status != Status.Healthy then Alpha.setStatus(pokemon, status)
            else Alpha.unit
        yield ()
  
      case CureStatus(pokemon)     => Alpha.setStatus(pokemon, Status.Healthy)