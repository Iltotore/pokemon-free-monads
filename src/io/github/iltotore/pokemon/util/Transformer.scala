package io.github.iltotore.pokemon.util

trait Transformer[M[_], G[_]]:
  def apply[A](m: M[A]): G[A]

infix type ~>[M[_], G[_]] = Transformer[M, G]