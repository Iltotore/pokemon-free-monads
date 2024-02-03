package io.github.iltotore.pokemon.util

trait NaturalTransformation[M[_], G[_]]:
  def apply[A](m: M[A]): G[A]

infix type ~>[M[_], G[_]] = NaturalTransformation[M, G]