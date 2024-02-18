package io.github.iltotore.pokemon.action

import io.github.iltotore.pokemon.{Game, WhichPlayer, WhichPokemon}
import io.github.iltotore.pokemon.move.Move

enum Theta:
  case UseMove(move: Move, user: WhichPokemon, target: WhichPokemon)
  case SwitchIn(player: WhichPlayer, slot: Int)
  case Cancel(program: Theta, instead: Beta[Unit])
  case Before(program: Theta, before: Beta[Unit])
  case Random(chance: Double, program: Theta)

  def affectedPokemon: WhichPokemon = this match
    case UseMove(move, user, target) => user
    case SwitchIn(player, slot)      => WhichPokemon.Active(player)
    case Cancel(program, _)          => program.affectedPokemon
    case Before(program, _)          => program.affectedPokemon
    case Random(chance, program)     => program.affectedPokemon

  def toBeta: Beta[Unit] = this match
    case UseMove(move, user, target) => move.effect(user, target)
    case SwitchIn(player, slot)      => Beta.switchIn(player, slot)
    case Cancel(program, instead)    => instead
    case Before(program, before) =>
      for
        _ <- before
        _ <- program.toBeta
      yield ()

    case Random(chance, program) => Beta.chooseRandom(chance, program.toBeta)

  def run(game: Game): Game =
    toBeta
      .foldMap(Beta.toAlpha)
      .foldMap(Alpha.toGameEffect)
      .run(game)
      ._1

  def compare(game: Game, to: Theta): Int = (this, to) match
    case (_: SwitchIn, _: (UseMove | Cancel)) => 1
    case (_: (UseMove | Cancel), _: SwitchIn) => -1
    case _ =>
      game.getPokemon(this.affectedPokemon).speed.compare(game.getPokemon(to.affectedPokemon).speed)
