package eu.derfniw.aoc2022.ex08

import scala.io.Source

import math.*

class TreeGrid(val grid: IndexedSeq[IndexedSeq[Int]]):
  val height = grid.size
  val width  = if height > 0 then grid(0).length else 0

  def treeIsVisible(x: Int, y: Int): Boolean =
    if x == 0 || y == 0 || x == width - 1 || y == height - 1 then true
    else
      val tree     = grid(y)(x)
      val fromWest = grid(y).slice(0, x).forall(_ < tree)
      val fromEast = grid(y).drop(x + 1).forall(_ < tree)

      val verticalLine = (0 until height).map(i => grid(i)(x))
      val fromNorth    = verticalLine.slice(0, y).forall(_ < tree)
      val fromSouth    = verticalLine.drop(y + 1).forall(_ < tree)
      fromWest || fromEast || fromNorth || fromSouth

  def treeScenicScore(x: Int, y: Int): Int =
    if x == 0 || y == 0 || x == width - 1 || y == height - 1 then 0
    else
      val tree = grid(y)(x)
      def traverseDirection(dir: Seq[Int], alternative: => Int): Int =
        val v = dir.indexWhere(_ >= tree)
        if v == -1 then alternative else v + 1

      val toWest = traverseDirection(grid(y).slice(0, x).reverse, x)
      val toEast = traverseDirection(grid(y).drop(x + 1), width - (x + 1))

      val verticalLine = (0 until height).map(i => grid(i)(x))
      val toNorth      = traverseDirection(verticalLine.slice(0, y).reverse, y)
      val toSouth      = traverseDirection(verticalLine.drop(y + 1), height - (y + 1))

      val result = toWest * toEast * toNorth * toSouth
      result
  end treeScenicScore

end TreeGrid

def parseInput(in: Source): TreeGrid =
  TreeGrid(in.getLines().map(_.map(_.asDigit)).toIndexedSeq)

def run01(in: Source): Int =
  val trees = parseInput(in)
  (for
    y <- (0 until trees.height)
    x <- (0 until trees.width)
  yield trees.treeIsVisible(x, y)).count(_ == true)
end run01

def run02(in: Source): Int =
  val trees = parseInput(in)
  (for
    y <- (0 until trees.height)
    x <- (0 until trees.width)
  yield trees.treeScenicScore(x, y)).max

@main
def exercise08() =
  def source = Source.fromResource("input_08")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
