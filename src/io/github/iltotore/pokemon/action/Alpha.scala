package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.*
import io.github.iltotore.pokemon.util.*

type Alpha[A] = Free[Alpha.Algebra, A]
object Alpha:
  
  enum Algebra[Out]:
    case GetHealth(pokemon: WhichPokemon) extends Algebra[Double]
    case GetMaxHealth(pokemon: WhichPokemon) extends Algebra[Double]
    case GetType(pokemon: WhichPokemon) extends Algebra[Type]
    case GetStatus(pokemon: WhichPokemon) extends Algebra[Status]
    case Random() extends Algebra[Double]
    case SetHealth(pokemon: WhichPokemon, health: Double) extends Algebra[Unit]
    case SetType(pokemon: WhichPokemon, tpe: Type) extends Algebra[Unit]
    case SetStatus(pokemon: WhichPokemon, status: Status) extends Algebra[Unit]
    case SetActivePokemon(player: WhichPlayer, slot: Int) extends Algebra[Unit]
    
  import Algebra.*

  def pure[A](value: A): Alpha[A] = Free.pure(value)
  
  val unit: Alpha[Unit] = pure(())
  
  def getHealth(pokemon: WhichPokemon): Alpha[Double] = Free.liftM(GetHealth(pokemon))

  def getMaxHealth(pokemon: WhichPokemon): Alpha[Double] = Free.liftM(GetMaxHealth(pokemon))

  def getType(pokemon: WhichPokemon): Alpha[Type] = Free.liftM(GetType(pokemon))

  def getStatus(pokemon: WhichPokemon): Alpha[Status] = Free.liftM(GetStatus(pokemon))
  
  def random(): Alpha[Double] = Free.liftM(Random())

  def setHealth(pokemon: WhichPokemon, health: Double): Alpha[Unit] = Free.liftM(SetHealth(pokemon, health))

  def setType(pokemon: WhichPokemon, tpe: Type): Alpha[Unit] = Free.liftM(SetType(pokemon, tpe))

  def setStatus(pokemon: WhichPokemon, status: Status): Alpha[Unit] = Free.liftM(SetStatus(pokemon, status))

  def setActivePokemon(player: WhichPlayer, slot: Int): Alpha[Unit] = Free.liftM(SetActivePokemon(player, slot))

  def decreaseHealth(pokemon: WhichPokemon, amount: Double): Alpha[Unit] =
    for
      health <- getHealth(pokemon)
      _      <- setHealth(pokemon, math.max(0, health - amount))
    yield ()
    
  def increaseHealth(pokemon: WhichPokemon, amount: Double): Alpha[Unit] =
    for
      health    <- getHealth(pokemon)
      maxHealth <- getMaxHealth(pokemon)
      _         <- setHealth(pokemon, math.min(maxHealth, health + amount))
    yield ()

  def toGameEffect: Algebra ~> GameEffect = new:

    override def apply[A](m: Algebra[A]): GameEffect[A] = game =>
      m match
        case GetHealth(pokemon) => (game, game.getPokemon(pokemon).currentHealth)
        case GetMaxHealth(pokemon) => (game, game.getPokemon(pokemon).maxHealth)
        case GetType(pokemon) => (game, game.getPokemon(pokemon).pokemonType)
        case GetStatus(pokemon) => (game, game.getPokemon(pokemon).status)
        case Random() => (game, game.random.nextGaussian())
        case SetHealth(pokemon, health) => (game.modifyPokemon(pokemon, _.copy(currentHealth = health)), ())
        case SetType(pokemon, tpe) => (game.modifyPokemon(pokemon, _.copy(pokemonType = tpe)), ())
        case SetStatus(pokemon, status) => (game.modifyPokemon(pokemon, _.copy(status = status)), ())
        case SetActivePokemon(player, slot) => (game.modifyPlayer(player, _.copy(activeSlot = slot)), ())
