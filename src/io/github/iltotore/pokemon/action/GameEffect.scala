package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.Game
import io.github.iltotore.pokemon.util.Monad

trait GameEffect[A]:

  def run(game: Game): (Game, A)

object GameEffect:

  given Monad[GameEffect] with

    override def pure[A](value: A): GameEffect[A] = game => (game, value)

    extension [A](monad: GameEffect[A])
      override def flatMap[B](f: A => GameEffect[B]): GameEffect[B] = game =>
        val (updated, value) = monad.run(game)
        f(value).run(updated)
