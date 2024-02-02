package io.github.iltotore.pokemon.util


enum Free[M[_], A]:
  case Pure(value: A)
  case FlatMap[M[_], A, B](free: Free[M, A], f: A => Free[M, B]) extends Free[M, B]
  case Suspend(monad: M[A])

  def flatMap[B](f: A => Free[M, B]): Free[M, B] = FlatMap(this, f)

  def map[B](f: A => B): Free[M, B] = this.flatMap[B](f andThen Free.pure)

  def foldMap[G[_]: Monad](natTrans: M ~> G): G[A] = this match
    case Pure(value) => summon[Monad[G]].pure(value)
    case FlatMap(free, f) =>
      free.foldMap(natTrans).flatMap(x => f(x).foldMap(natTrans))
    case Suspend(monad) => natTrans(monad)

  def rewrite(rule: PartialFunction[Free[M, ?], Free[M, ?]]): Free[M, A] = this match
    case free: Free[M, ?] if rule.isDefinedAt(free) => rule(free).asInstanceOf[Free[M, A]]
    case FlatMap(free, f) => FlatMap(free.rewrite(rule), x => f(x).rewrite(rule))
    case _ => this

object Free:

  def pure[M[_], A](value: A): Free[M, A] = Free.Pure(value)

  def liftM[M[_], A](monad: M[A]): Free[M, A] = Free.Suspend(monad)