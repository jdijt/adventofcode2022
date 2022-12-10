package eu.derfniw.aoc2022.ex09

import scala.annotation.tailrec
import scala.collection.View.Unfold
import scala.io.Source

import math.*

enum Move:
  case Up(steps: Int)
  case Down(steps: Int)
  case Right(steps: Int)
  case Left(steps: Int)

  def steps: Int

object Move:
  def fromLine(line: String) = line match
    case s"U $steps" => Up(steps.toInt)
    case s"D $steps" => Down(steps.toInt)
    case s"R $steps" => Right(steps.toInt)
    case s"L $steps" => Left(steps.toInt)

case class Point(x: Int, y: Int):
  def translate(xChange: Int, yChange: Int) = Point(x + xChange, y + yChange)

  def adjacent(other: Point): Boolean =
    abs(this.x - other.x) <= 1 && abs(this.y - other.y) <= 1

  def followOther(other: Point): Point =
    if this.adjacent(other) then this
    else
      if this.x == other.x then this.translate(0, if other.y < this.y then -1 else 1)
      if this.y == other.y then this.translate(if other.x < this.x then -1 else 1, 0)
      else
        val xMove = if other.x < this.x then -1 else 1
        val yMove = if other.y < this.y then -1 else 1
        this.translate(xMove, yMove)
end Point

class Grid(head: Point, tail: Seq[Point], val tailVisited: Set[Point]):
  import Move.*

  @tailrec
  final def applyMove(move: Move): Grid =
    if move.steps == 0 then this
    else
      val (newHead, nextMove) = move match
        case Up(m)    => (head.translate(0, 1), Up(m - 1))
        case Down(m)  => (head.translate(0, -1), Down(m - 1))
        case Right(m) => (head.translate(1, 0), Right(m - 1))
        case Left(m)  => (head.translate(-1, 0), Left(m - 1))
      val newTail = Unfold((tail, newHead)) {
        case (Seq(), _) => None
        case (toTranslate +: remainder, lastTranslated) =>
          val translated = toTranslate.followOther(lastTranslated)
          Some((translated, (remainder, translated)))
      }.toSeq

      Grid(newHead, newTail, tailVisited + newTail.last).applyMove(nextMove)
end Grid

def run01(in: Source): Int =
  in.getLines()
    .map(Move.fromLine)
    .foldLeft(Grid(Point(0, 0), Seq(Point(0, 0)), Set(Point(0, 0)))) { (grid, move) =>
      grid.applyMove(move)
    }
    .tailVisited
    .size
end run01

def run02(in: Source): Int =
  in.getLines()
    .map(Move.fromLine)
    .foldLeft(Grid(Point(0, 0), Seq.fill(9)(Point(0, 0)), Set(Point(0, 0)))) { (grid, move) =>
      grid.applyMove(move)
    }
    .tailVisited
    .size

@main
def exercise09() =
  def source = Source.fromResource("input_09")
  print("Part 1: ")
  println(run01(source))
//print("Part 2: ")
//println(run02(source))
