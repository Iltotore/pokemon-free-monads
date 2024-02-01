package io.github.iltotore.pokemon

enum Effectiveness:
  case Weak
  case Neutral
  case Resisted
  case Immune

  def coefficient: Double = this match
    case Weak     => 2
    case Neutral  => 1
    case Resisted => 0.5
    case Immune   => 0
