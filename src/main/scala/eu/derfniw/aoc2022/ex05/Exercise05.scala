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
  def update1(move: Move): Stacks =
    val boxes        = stacks(move.from).takeRight(move.amount)
    val removedBoxes = stacks + (move.from -> stacks(move.from).dropRight(move.amount))
    val reAddBoxes   = removedBoxes + (move.to -> (removedBoxes(move.to) ++ boxes.reverse))
    Stacks(reAddBoxes)

  def update2(move: Move): Stacks =
    val boxes        = stacks(move.from).takeRight(move.amount)
    val removedBoxes = stacks + (move.from -> stacks(move.from).dropRight(move.amount))
    val reAddBoxes   = removedBoxes + (move.to -> (removedBoxes(move.to) ++ boxes))
    Stacks(reAddBoxes)
// format: on
end Stacks

object InputParser extends RegexParsers:
  override def skipWhitespace: Boolean = false

  def number: Parser[Int]      = "[0-9]+".r ^^ { num => num.toInt }
  def boxLabel: Parser[String] = "[A-Z]".r

  def stackItem: Parser[String] = "[" ~> boxLabel <~ "]"
  def stackLayer: Parser[Seq[Option[String]]] =
    rep((stackItem | "   ") <~ " ".?) ^^ { case itemList =>
      itemList.map {
        case "   "     => None
        case s: String => Some(s)
      }
    }
  def stackLayers: Parser[Seq[Seq[Option[String]]]] = rep(stackLayer <~ "\n")

  def stackLabel: Parser[Int]           = " " ~> number <~ " ".?
  def stackLabelLine: Parser[List[Int]] = rep(stackLabel <~ " ".?) <~ "\n"

  def move: Parser[Move] = "move " ~> number ~ " from " ~ number ~ " to " ~ number ^^ {
    case amount ~ _ ~ from ~ _ ~ to => Move(amount, from, to) // Correct to 0 index
  }
  def moves: Parser[Seq[Move]] = rep(move <~ "\n") ~ move ^^ { case moves ~ move => moves :+ move }

  def wholeFile: Parser[(Seq[Seq[Option[String]]], Seq[Int], Seq[Move])] =
    // Reverse the layers to make index 0 the bottom layer.
    stackLayers ~ stackLabelLine ~ "\n" ~ moves ^^ { case layers ~ labels ~ _ ~ moves =>
      (layers.reverse, labels, moves)
    }

  def parseStacks(in: String): (Stacks, Seq[Move]) =
    val (parsedLayers, parsedLabels, moves) = parse(wholeFile, in).get

    val stacks = Unfold(parsedLabels) {
      case Nil => None
      case l :: ls =>
        Some((l -> parsedLayers.map(layer => layer(l - 1)).flatten, ls))
    }.toSeq

    (Stacks(SortedMap(stacks.toArray*)), moves)
  end parseStacks
end InputParser

def run01(input: Source): String =
  val (stacks, moves) = InputParser.parseStacks(input.mkString)

  val result = moves.foldLeft(stacks)((s, move) => s.update1(move))
  result.stacks.map { case (_, v) => v.last }.mkString

def run02(input: Source): String =
  val (stacks, moves) = InputParser.parseStacks(input.mkString)

  val result = moves.foldLeft(stacks)((s, move) => s.update2(move))
  result.stacks.map { case (_, v) => v.last }.mkString

@main
def exercise04() =
  def source = Source.fromResource("input_05")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
