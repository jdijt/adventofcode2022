package eu.derfniw.aoc2022.ex01

import scala.annotation.tailrec
import scala.io.Source

type ElfLoad = Int

def parseInput(in: Source): Seq[ElfLoad] =
  @tailrec
  def groupingHelper(in: Seq[Option[Int]], loads: Seq[ElfLoad]): Seq[ElfLoad] =
    in.dropWhile(_.isEmpty) match
      case Seq() => loads
      case cs =>
        val elf = cs.takeWhile(_.nonEmpty).flatten.sum
        groupingHelper(cs.dropWhile(_.nonEmpty), loads :+ elf)

  val lines = in.mkString.split("\n").map(_.toIntOption)
  groupingHelper(lines, Seq())
end parseInput

def run01(input: Source): Int =
  parseInput(input).max

def run02(input: Source): Int =
  parseInput(input).sorted.takeRight(3).sum

@main
def exercise01() =
  def source = Source.fromResource("input_01")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
