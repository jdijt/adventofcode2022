package eu.derfniw.aoc2022.ex01

import scala.annotation.tailrec
import scala.io.Source

type ElfLoad = Seq[Int]

def parseInput(in: Source): Seq[ElfLoad] = 
  @tailrec
  def groupingHelper(in: Seq[Option[Int]], loads: Seq[ElfLoad]): Seq[ElfLoad] =
    in.dropWhile(_.isEmpty) match
      case Seq() => loads
      case cs =>
        val elf = cs.takeWhile(_.nonEmpty).flatten
        groupingHelper(cs.drop(elf.length), loads :+ elf)
  
  val lines = in.mkString.split("\n").map(_.toIntOption)
  groupingHelper(lines, Seq())

def run01(input: Source): Int =
  parseInput(input).map(_.sum).max

def run02(input: Source): Int =
  parseInput(input).map(_.sum).sorted.takeRight(3).sum

@main
def exercise01() =
  print("Part 1: ")
  println(run01(Source.fromResource("input_01")))
  print("Part 2: ")
  println(run02(Source.fromResource("input_01")))
