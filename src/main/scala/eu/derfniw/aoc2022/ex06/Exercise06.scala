package eu.derfniw.aoc2022.ex06

import scala.io.Source

def firstUniqueSetEndIndex(in: String, length: Int): Option[Int] =
  in.sliding(length)
    .zip(length to in.length)
    .find((marker, _) => marker.toSet.size == length)
    .map(_._2)

def run01(input: Source): Int = firstUniqueSetEndIndex(input.mkString, 4).get

def run02(input: Source): Int =
  firstUniqueSetEndIndex(input.mkString, 14).get

@main
def exercise04() =
  def source = Source.fromResource("input_06")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
