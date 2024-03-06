package io.github.iltotore.pokemon.util

enum Free[M[_], A]:
  case Pure(value: A)
  case FlatMap[M[_], A, B](free: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
  case Suspend(monad: M[A])

  def foldMap[G[_]: Monad](natTrans: M ~> G): G[A] = this match
    case Pure(value) => summon[Monad[G]].pure(value)
    case FlatMap(free, f) =>
      free.foldMap(natTrans).flatMap(x => f(x).foldMap(natTrans))
    case Suspend(monad) => natTrans(monad)

  def rewrite[B](rule: PartialFunction[M[B], Free[M, B]]): Free[M, A] = this match
    case Suspend(monad: M[B] @unchecked) if rule.isDefinedAt(monad) => rule(monad).asInstanceOf[Free[M, A]]
    case FlatMap(free, f)                                           => FlatMap(free.rewrite(rule), x => f(x).rewrite(rule))
    case _                                                          => this

object Free:

  def pure[M[_], A](value: A): Free[M, A] = Pure(value)

  def liftM[M[_], A](monad: M[A]): Free[M, A] = Suspend(monad)

  given [M[_]]: Monad[[x] =>> Free[M, x]] with

    override def pure[A](value: A): Free[M, A] = Pure(value)

    extension [A](monad: Free[M, A])
      override def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(monad, f)

      override def map[B](f: A => B): Free[M, B] = flatMap(f andThen pure)
