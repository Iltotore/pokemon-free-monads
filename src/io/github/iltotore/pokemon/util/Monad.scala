package io.github.iltotore.pokemon.util

trait Monad[M[_]]:

  def pure[A](value: A): M[A]

  extension [A](monad: M[A])

    def flatMap[B](f: A => M[B]): M[B]
    
    def map[B](f: A => B): M[B] = flatMap(f andThen pure)