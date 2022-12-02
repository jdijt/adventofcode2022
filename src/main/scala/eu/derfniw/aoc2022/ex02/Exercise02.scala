package eu.derfniw.aoc2022.ex02

import scala.annotation.tailrec
import scala.io.Source

enum Result(val score: Int):
  case Lose extends Result(0)
  case Draw extends Result(3)
  case Win  extends Result(6)

  import Move.*
  def getNeededMoveForResult(opponent: Move): Move = this match
    case Result.Draw => opponent
    case Result.Lose =>
      opponent match
        case Rock     => Scissors
        case Paper    => Rock
        case Scissors => Paper
    case Result.Win =>
      opponent match
        case Rock     => Paper
        case Paper    => Scissors
        case Scissors => Rock
end Result

object Result:
  def apply(in: Char): Result = in.toLower match
    case 'x' => Lose
    case 'y' => Draw
    case 'z' => Win

enum Move(val score: Int):
  case Rock     extends Move(1)
  case Paper    extends Move(2)
  case Scissors extends Move(3)

  import Result.*
  def matchResult(other: Move): Result = (this, other) match
    case (Rock, Scissors)  => Win
    case (Scissors, Paper) => Win
    case (Paper, Rock)     => Win
    case (a, b) if a == b  => Draw
    case _                 => Lose

end Move

object Move:
  def apply(in: Char): Move = in.toLower match
    case 'a' | 'x' => Rock
    case 'b' | 'y' => Paper
    case 'c' | 'z' => Scissors

class Match(opponent: Move, mine: Move):
  def score = mine.matchResult(opponent).score + mine.score

def parseInput01(in: Source): Seq[Match] =
  in.mkString.split("\n").map(line => Match(Move(line(0)), Move(line(2))))

def parseInput02(in: Source): Seq[Match] =
  in.mkString
    .split("\n")
    .map(line =>
      val opponent = Move(line(0))
      val mine     = Result(line(2)).getNeededMoveForResult(opponent)
      Match(opponent, mine)
    )

def run01(input: Source): Int =
  parseInput01(input).map(_.score).sum

def run02(input: Source): Int =
  parseInput02(input).map(_.score).sum

@main
def exercise02() =
  def source = Source.fromResource("input_02")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
