package eu.derfniw.aoc2022.ex01

import scala.annotation.tailrec
import scala.io.Source

@tailrec
def groupingHelper(in: Seq[Option[Int]], groups: Seq[Seq[Int]]): Seq[Seq[Int]] =
  in.dropWhile(_.isEmpty) match
    case Seq() => groups
    case cs =>
      val group = cs.takeWhile(_.nonEmpty).map(_.get)
      groupingHelper(cs.dropWhile(_.nonEmpty), groups :+ group)

def run01(input: Source): Int =
  val lines    = input.mkString.split("\n").map(_.toIntOption)
  val elfLoads = groupingHelper(lines, Seq())
  elfLoads.map(_.sum).max

def run02(input: Source): Int =
  val lines = input.mkString.split("\n").map(_.toIntOption)
  val elfLoads = groupingHelper(lines, Seq())
  elfLoads.map(_.sum).sorted.takeRight(3).sum

@main
def exercise01() =
  print("Part 1: ")
  println(run01(Source.fromResource("input_01")))
  print("Part 2: ")
  println(run02(Source.fromResource("input_01")))
