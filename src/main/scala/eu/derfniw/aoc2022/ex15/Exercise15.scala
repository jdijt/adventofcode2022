package eu.derfniw.aoc2022.ex15

import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

import math.*

case class Point(x: Int, y: Int):
  def manhattanDist(other: Point): Int =
    abs(this.x - other.x) + abs(this.y - other.y)

case class Sensor(location: Point, beacon: Point):
  val beaconDistance = location.manhattanDist(beacon)
  val minX           = location.x - beaconDistance
  val maxX           = location.x + beaconDistance
  val minY           = location.y - beaconDistance
  val maxY           = location.y + beaconDistance

  def inRange(other: Point): Boolean = other.manhattanDist(location) <= beaconDistance
end Sensor

def parseInput(in: Source): Seq[Sensor] =
  in.getLines()
    .map { case s"Sensor at x=$sX, y=$sY: closest beacon is at x=$bX, y=$bY" =>
      Sensor(Point(sX.toInt, sY.toInt), Point(bX.toInt, bY.toInt))
    }
    .toSeq

def run01(rowToCheck: Int)(in: Source): Int =
  val sensors    = parseInput(in)
  val globalMinX = sensors.map(_.minX).min
  val globalMaxX = sensors.map(_.maxX).max

  (globalMinX to globalMaxX).par.map { x =>
    val point          = Point(x, rowToCheck)
    val sensorsInRange = sensors.filter(_.inRange(point))
    if sensorsInRange.isEmpty || sensorsInRange.exists(_.beacon == point) then 0
    else 1
  }.sum

end run01

def run02(space: Int)(in: Source): Long =
  val sensors = parseInput(in)

  val point = sensors.toSet.par.flatMap { s =>
    ((s.minX - 1) to (s.maxX + 1)).iterator
      .flatMap { x =>
        val yOffset = (s.beaconDistance + 1) - abs(s.location.x - x)
        if yOffset == 0 then Seq(Point(x, s.location.y))
        else Seq(Point(x, s.location.y + yOffset), Point(x, s.location.y - yOffset))
      }
      .filter(p => p.x <= space && p.x >= 0 && p.y <= space && p.y >= 0)
      .filter(p => !sensors.exists(_.inRange(p)))
  }.head // This should lead to only one unique point. Per challenge design.

  point.x * 4000000L + point.y
end run02

@main
def exercise15() =
  def source = Source.fromResource("input_15")
  print("Part 1: ")
  println(run01(2000000)(source))
  print("Part 2: ")
  println(run02(4000000)(source))
