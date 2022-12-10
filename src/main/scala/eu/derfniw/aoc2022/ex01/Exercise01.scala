package eu.derfniw.aoc2022.ex01

import scala.annotation.tailrec
import scala.collection.View.Unfold
import scala.io.Source

import java.util.stream.Collector.Characteristics

type ElfLoad = Int

def parseInput(in: Source): Seq[ElfLoad] =
  Unfold(in.getLines().map(_.toIntOption).dropWhile(_.isEmpty)) { cs =>
    if cs.isEmpty then None
    else
      val elf       = cs.takeWhile(_.nonEmpty).flatten.sum
      val remainder = cs.dropWhile(_.isEmpty)
      Some((elf, remainder))
  }.toSeq
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
