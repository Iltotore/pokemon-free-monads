package io.github.iltotore.pokemon

import io.github.iltotore.pokemon.util.Monad

case class GameEffect[A](run: Game => (Game, A))

object GameEffect:

  given Monad[GameEffect] with

    override def pure[A](value: A): GameEffect[A] = GameEffect(game => (game, value))

    extension [A](monad: GameEffect[A]) override def flatMap[B](f: A => GameEffect[B]): GameEffect[B] = GameEffect: game =>
      val (state, result) = monad.run(game)
      f(result).run(state)