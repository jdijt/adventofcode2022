package eu.derfniw.aoc2022.ex05

import scala.annotation.tailrec
import scala.collection.View.Unfold
import scala.collection.immutable.SortedMap
import scala.io.Source
import scala.util.Try
import scala.util.parsing.combinator.RegexParsers

case class Move(amount: Int, from: Int, to: Int)

class Stacks(val stacks: Map[Int, Seq[String]]):

  // format: off
  def update9000(move: Move): Stacks =
    val boxes        = stacks(move.from).takeRight(move.amount)
    val removedBoxes = stacks + (move.from -> stacks(move.from).dropRight(move.amount))
    val reAddBoxes   = removedBoxes + (move.to -> (removedBoxes(move.to) ++ boxes.reverse))
    Stacks(reAddBoxes)

  def update9001(move: Move): Stacks =
    val boxes        = stacks(move.from).takeRight(move.amount)
    val removedBoxes = stacks + (move.from -> stacks(move.from).dropRight(move.amount))
    val reAddBoxes   = removedBoxes + (move.to -> (removedBoxes(move.to) ++ boxes))
    Stacks(reAddBoxes)
  // format: on

  def message = stacks.map((_, v) => v.lastOption).flatten.mkString
end Stacks

object InputParser extends RegexParsers:
  override def skipWhitespace: Boolean = false

  def number: Parser[Int] = "[0-9]+".r ^^ { num => num.toInt }

  def stackLayer: Parser[Seq[Option[String]]] =
    repsep(("[" ~> "[A-Z]".r <~ "]" | "   "), " ") ^^ { case itemList =>
      itemList.map {
        case "   "     => None
        case s: String => Some(s)
      }
    }

  def stackLabelLine: Parser[List[Int]] = repsep(" " ~> number <~ " ", " ") <~ "\n"

  def move: Parser[Move] = "move " ~> number ~ " from " ~ number ~ " to " ~ number ^^ {
    case amount ~ _ ~ from ~ _ ~ to => Move(amount, from, to)
  }

  def wholeFile: Parser[(Seq[Seq[Option[String]]], Seq[Int], Seq[Move])] =
    rep(stackLayer <~ "\n") ~ stackLabelLine ~ "\n" ~ repsep(move, "\n") ^^ {
      case layers ~ labels ~ _ ~ moves =>
        // Reverse the layers to make index 0 the bottom layer.
        (layers.reverse, labels, moves)
    }

  def parseStacks(in: String): (Stacks, Seq[Move]) =
    val (parsedLayers, parsedLabels, moves) = parse(wholeFile, in).get
    val stacks = parsedLabels.zipWithIndex
      .map((label, idx) => (label -> parsedLayers.map(layer => layer(idx)).flatten))
      .toSeq
    (Stacks(SortedMap(stacks*)), moves)

end InputParser

def run01(input: Source): String =
  val (stacks, moves) = InputParser.parseStacks(input.mkString)
  moves.foldLeft(stacks)((s, move) => s.update9000(move)).message

def run02(input: Source): String =
  val (stacks, moves) = InputParser.parseStacks(input.mkString)
  moves.foldLeft(stacks)((s, move) => s.update9001(move)).message

@main
def exercise04() =
  def source = Source.fromResource("input_05")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
