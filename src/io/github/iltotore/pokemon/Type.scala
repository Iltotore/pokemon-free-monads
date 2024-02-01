package io.github.iltotore.pokemon

enum Type:
  case Normal
  case Grass
  case Fire
  case Water
  case Ghost
  case Electric

  def weaknesses: List[Type] = this match
    case Normal   => List.empty
    case Grass    => List(Fire)
    case Fire     => List(Water)
    case Water    => List(Grass)
    case Ghost    => List(Ghost)
    case Electric => List.empty

  def resistances: List[Type] = this match
    case Normal   => List.empty
    case Grass    => List(Grass, Water)
    case Fire     => List(Fire, Grass)
    case Water    => List(Water, Fire)
    case Ghost    => List.empty
    case Electric => List.empty

  def immunities: List[Type] = this match
    case Normal   => List(Ghost)
    case Grass    => List.empty
    case Fire     => List.empty
    case Water    => List.empty
    case Ghost    => List(Normal)
    case Electric => List.empty

  def effectiveness(of: Type): Effectiveness =
    if weaknesses.contains(of) then Effectiveness.Weak
    else if resistances.contains(of) then Effectiveness.Resisted
    else if immunities.contains(of) then Effectiveness.Immune
    else Effectiveness.Neutral
