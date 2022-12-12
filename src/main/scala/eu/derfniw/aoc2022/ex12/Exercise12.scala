package eu.derfniw.aoc2022.ex12

import scala.annotation.tailrec
import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

type HeightMap = IndexedSeq[IndexedSeq[Char]]

case class Point(x: Int, y: Int)

extension (m: HeightMap)
  def height = m.length
  def width  = if m.height > 0 then m(0).length else 0

  def findPoint(value: Char): Point =
    val y = m.indexWhere(_.contains(value))
    val x = m(y).indexOf(value)
    Point(x, y)

  def valueAt(point: Point): Char = m(point.y)(point.x)

  def reachableFrom(point: Point): Set[Point] =
    val candidates = Seq(
      Point(point.x - 1, point.y),
      Point(point.x + 1, point.y),
      Point(point.x, point.y - 1),
      Point(point.x, point.y + 1)
    )

    candidates
      .filterNot(p =>
        p.x < 0
          || p.y < 0
          || p.x >= m.width
          || p.y >= m.height
          || normalizePointHeight(m.valueAt(p)) - normalizePointHeight(m.valueAt(point)) > 1
      )
      .toSet
  end reachableFrom
end extension

def normalizePointHeight(value: Char): Char =
  if value == 'S' then 'a' else if value == 'E' then 'z' else value

def parseInput(in: Source): HeightMap = in.getLines().map(_.toIndexedSeq).toIndexedSeq

def dijkstraHelper(startPoint: Point, endPoint: Point, map: HeightMap): Int =
  @tailrec
  def helper(knownDistances: Map[Point, Int], visitedPoints: Set[Point]): Int =
    val pointOpt = knownDistances.removedAll(visitedPoints).minByOption(_._2)
    if pointOpt.isEmpty then Int.MaxValue
    else
      val (point, pathLength) = pointOpt.get
      val neighbors           = map.reachableFrom(point).removedAll(visitedPoints)
      val newKnownDistances = neighbors.foldLeft(knownDistances) { (dists, p) =>
        dists + (p -> (pathLength + 1))
      }
      if neighbors.contains(endPoint) then newKnownDistances(endPoint)
      else helper(newKnownDistances, visitedPoints + point)
  end helper

  val startingKnownDistances = Map(startPoint -> 0).withDefaultValue(Int.MaxValue)
  helper(startingKnownDistances, Set())

end dijkstraHelper

def run01(in: Source): Int =
  val map        = parseInput(in)
  val startPoint = map.findPoint('S')
  val endPoint   = map.findPoint('E')
  dijkstraHelper(startPoint, endPoint, map)
end run01

def run02(in: Source): Int =
  val map      = parseInput(in)
  val endPoint = map.findPoint('E')
  val startPoints = map.zipWithIndex.flatMap { (row, y) =>
    row.zipWithIndex.flatMap { (value, x) =>
      if value == 'a' || value == 'S' then Some(Point(x, y)) else None
    }
  }
  startPoints.par.map(p => dijkstraHelper(p, endPoint, map)).min
end run02

@main
def exercise10() =
  def source = Source.fromResource("input_12")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
