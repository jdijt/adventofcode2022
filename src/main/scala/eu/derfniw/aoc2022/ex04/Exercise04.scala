package eu.derfniw.aoc2022.ex04

import scala.annotation.tailrec
import scala.io.Source

class SectionRange(val from: Int, val to: Int):
  private def isInRange(otherPoint: Int) =
    otherPoint >= from && otherPoint <= to

  def contains(other: SectionRange): Boolean =
    isInRange(other.from) && isInRange(other.to)

  def overlaps(other: SectionRange): Boolean =
    isInRange(other.from) || isInRange(other.to) || other.contains(this)

end SectionRange

def parseInput(in: Source): Seq[(SectionRange, SectionRange)] =
  in.getLines()
    .map { case s"$a-$b,$x-$y" =>
      (SectionRange(a.toInt, b.toInt), SectionRange(x.toInt, y.toInt))
    }
    .toSeq

def run01(input: Source): Int =
  parseInput(input).count { case (sra, srb) =>
    sra.contains(srb) || srb.contains(sra)
  }

def run02(input: Source): Int =
  parseInput(input).count { case (sra, srb) =>
    sra.overlaps(srb)
  }

@main
def exercise04() =
  def source = Source.fromResource("input_04")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
