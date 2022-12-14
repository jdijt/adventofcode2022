package eu.derfniw.aoc2022.ex14

import scala.annotation.tailrec
import scala.io.Source

import math.*

case class Point(x: Int, y: Int):
  def pointsBetween(other: Point): Set[Point] =
    if this.x == other.x then
      (min(this.y, other.y) to max(this.y, other.y)).map(newY => Point(this.x, newY)).toSet
    else if this.y == other.y then
      (min(this.x, other.x) to max(this.x, other.x)).map(newX => Point(newX, this.y)).toSet
    else throw new RuntimeException("Cannot do diagonal lines")

  def moveDown          = this.copy(y = y + 1)
  def moveDiagonalLeft  = Point(x - 1, y + 1)
  def moveDiagonalRight = Point(x + 1, y + 1)
end Point

case class Cave(rockPoints: Set[Point], sandPoints: Set[Point] = Set()):
  val sandSource = Point(500, 0)
  def minX       = min(rockPoints.map(_.x).min, sandPoints.map(_.x).min)
  def maxX       = max(rockPoints.map(_.x).max, sandPoints.map(_.x).max)
  def minY       = 0 // sandSource
  def maxY       = rockPoints.map(_.y).max
  def floor      = maxY + 2

  def pointAvailable(p: Point): Boolean =
    !(rockPoints.contains(p) || sandPoints.contains(p)) && p.y != floor

  def canLand(p: Point): Boolean = p.y < maxY

  def addSand: Cave =
    @tailrec
    def addSandHelper(currentSand: Point): Cave =
      if !canLand(currentSand) then this
      else
        val down      = currentSand.moveDown
        val diagonalL = currentSand.moveDiagonalLeft
        val diagonalR = currentSand.moveDiagonalRight
        if pointAvailable(down) then addSandHelper(down)
        else if pointAvailable(diagonalL) then addSandHelper(diagonalL)
        else if pointAvailable(diagonalR) then addSandHelper(diagonalR)
        else Cave(rockPoints, sandPoints + currentSand)

    addSandHelper(sandSource)
  end addSand

  def addSandWithFloor: Cave =
    @tailrec
    def addSandHelper(currentSand: Point): Cave =
      val down      = currentSand.moveDown
      val diagonalL = currentSand.moveDiagonalLeft
      val diagonalR = currentSand.moveDiagonalRight
      if pointAvailable(down) then addSandHelper(down)
      else if pointAvailable(diagonalL) then addSandHelper(diagonalL)
      else if pointAvailable(diagonalR) then addSandHelper(diagonalR)
      else Cave(rockPoints, sandPoints + currentSand)

    addSandHelper(sandSource)
  end addSandWithFloor

  def mkString: String =
    (minY to floor)
      .map { y =>
        (minX to maxX).map { x =>
          val p = Point(x, y)
          if rockPoints(p) || p.y == floor then '#' else if sandPoints.contains(p) then 'o' else '.'
        }.mkString
      }
      .mkString("\n")
end Cave

def readInput(in: Source): Cave =
  val points = in
    .getLines()
    .map { l =>
      val route = l.split(" -> ").map { case s"$x,$y" => Point(x.toInt, y.toInt) }
      route.zip(route.drop(1)).flatMap((p1, p2) => p1.pointsBetween(p2)).toSet
    }
    .toSet
    .flatten
  Cave(points)
end readInput

def run01(in: Source): Int =
  val cave = readInput(in)

  @tailrec
  def dropSand(current: Cave): Int =
    val newCave = current.addSand
    if newCave == current then newCave.sandPoints.size
    else dropSand(newCave)

  dropSand(cave)
end run01

def run02(in: Source): Int =
  val cave = readInput(in)

  @tailrec
  def dropSand(current: Cave): Int =
    val newCave = current.addSandWithFloor
    if newCave == current then newCave.sandPoints.size
    else dropSand(newCave)

  dropSand(cave)
end run02

@main
def exercise14() =
  def source = Source.fromResource("input_14")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
