package eu.derfniw.aoc2022.ex03

import scala.annotation.tailrec
import scala.io.Source

type Item = Char

val itemPriorities =
  ('a' to 'z').zip((1 to 26)).toMap
    ++ ('A' to 'Z').zip(27 to 52).toMap

extension (i: Item) def priority: Int = itemPriorities(i)

def run01(input: Source): Int =
  input
    .getLines()
    .map { line =>
      val (p1, p2) = line.splitAt(line.length() / 2)
      p1.toSet.intersect(p2.toSet).head.priority
    }
    .sum

def run02(input: Source): Int =
  input
    .getLines()
    .map(_.toSet)
    .grouped(3)
    .map(_.reduce(_ intersect _).head.priority)
    .sum

@main
def exercise02() =
  def source = Source.fromResource("input_03")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
