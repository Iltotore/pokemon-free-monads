package io.github.iltotore.pokemon.tui

import io.github.iltotore.pokemon.action.Theta
import io.github.iltotore.pokemon.{Game, Player, Pokemon, WhichPlayer, WhichPokemon}
import io.github.iltotore.pokemon.move.Move

import scala.io.StdIn
import scala.util.boundary
import scala.util.boundary.break

def showHealthBar(currentHealth: Double, maxHealth: Double, length: Int): String =
  val percent = currentHealth / maxHealth
  val progress = (percent * length).toInt
  val emptySpace = length - progress
  s"[${"=" * progress}${" " * (length - progress)}] ${(percent * 100).toInt}%"

def showMove(move: Move): String = s"${move.name} (${move.tpe})"

def showActivePokemon(pokemon: Pokemon): String =
  val moves =
    pokemon
      .moves
      .map(m => s"- ${showMove(m)}")
      .mkString("\n")

  s"""${pokemon.species} (${pokemon.pokemonType})
     |${showHealthBar(pokemon.currentHealth, pokemon.maxHealth, 20)} | ${pokemon.status}
     |Ability: ${pokemon.ability.name}
     |Speed: ${pokemon.speed}
     |Moves:
     |$moves""".stripMargin

def showTeam(pokemons: List[Pokemon]): String =
  pokemons
    .zipWithIndex
    .map((p, i) => s"[$i] ${p.species} (${p.currentHealth}/${p.maxHealth} ${p.status})")
    .mkString(" | ")

def showPlayer(player: Player): String =
  s"""Player: ${player.name}
     |${showTeam(player.team)}
     |
     |${showActivePokemon(player.activePokemon)}""".stripMargin

def showState(game: Game): String =
  val players =
    game
      .players
      .map((key, value) => showPlayer(value))
      .mkString("\n\n---\n\n")

  s"""=== Turn ${game.turn} ===
     |$players""".stripMargin

def showSelectionMenu(player: Player): String =
  s"""What should ${player.activePokemon.species} (${player.name}) do?
     |- move <slot or name> <target slot or name>
     |- switch <slot>""".stripMargin

def getMoveAt(slot: Int, moves: List[Move]): Option[Move] = moves.lift(slot)

def getMoveByName(name: String, moves: List[Move]): Option[Move] =
  moves
    .find(_.name.replace(" ", "").equalsIgnoreCase(name.replace(" ", "")))

def getMove(slotOrName: String, moves: List[Move]): Either[String, Move] =
  slotOrName
    .toIntOption
    .flatMap(getMoveAt(_, moves))
    .toRight(s"No move at slot $slotOrName")
    .orElse(getMoveByName(slotOrName, moves).toRight(s"No move named $slotOrName"))

def getTargetPlayer(name: String, game: Game): Either[String, WhichPokemon] = name match
  case "A" | "a" => Right(WhichPokemon.Active(WhichPlayer.PlayerA))
  case "B" | "b" => Right(WhichPokemon.Active(WhichPlayer.PlayerB))
  case _ =>
    game
      .players
      .collectFirst:
        case (w, p) if p.name.replace(" ", "").equalsIgnoreCase(name.replace(" ", "")) => WhichPokemon.Active(w)
      .toRight(s"No player named $name")

def parseSwitch(which: WhichPlayer, player: Player, slot: String): Either[String, Theta] =
  slot
    .toIntOption
    .toRight(s"Invalid int: $slot")
    .filterOrElse(i => i >= 0 && i < player.team.length, s"No pokemon at slot $slot")
    .filterOrElse(_ != player.activeSlot, s"${player.activePokemon.species} is already on the terrain")
    .filterOrElse(player.team(_).alive, "This pokemon is fainted")
    .map(i => Theta.SwitchIn(which, i))

def parseAction(which: WhichPlayer, game: Game, action: String): Either[String, Theta] =
  val player = game.players(which)
  val active = player.activePokemon

  action match
    case s"move $move, $target" =>
      for
        move <- getMove(move, active.moves)
        target <- getTargetPlayer(target, game)
      yield Theta.UseMove(move, WhichPokemon.Active(which), target)

    case s"switch $slot" =>
      parseSwitch(which, player, slot)

    case _ => Left(s"No action for $action")

def ask[A](message: String, parse: String => Either[String, A]): A =
  println(message)
  boundary[A]:
    while true do
      parse(StdIn.readLine()) match
        case Left(error)  => println(error)
        case Right(value) => break(value)

    ???
