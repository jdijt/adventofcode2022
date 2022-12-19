package eu.derfniw.aoc2022.ex16

import scala.collection.parallel.CollectionConverters.*
import scala.io.Source

case class Valve(id: String, flow: Int, neighbours: Set[String])

def parseInput(in: Source): Map[String, Valve] =
  in.getLines()
    .map {
      case s"Valve $id has flow rate=$flow; tunnels lead to valves $valves" =>
        (id -> Valve(id, flow.toInt, valves.split(",").map(_.trim()).toSet))
      case s"Valve $id has flow rate=$flow; tunnel leads to valve $valve" =>
        (id -> Valve(id, flow.toInt, Set(valve)))
    }
    .toMap

def run01(in: Source): Int =
  val valves = parseInput(in)
  val memory = scala.collection.mutable.Map[(Valve, Set[String], Int, Int), Int]()
  def traverseValves(current: Valve, opened: Set[String], stepsLeft: Int, currentFlow: Int): Int =
    val cacheKey = (current, opened, stepsLeft, currentFlow)
    if memory.contains(cacheKey) then memory(cacheKey)
    else if stepsLeft == 0 then
      memory.put(cacheKey, 0)
      0
    else
      val result =
        if opened(current.id) || stepsLeft == 1 then
          current.neighbours.map { id =>
            currentFlow + traverseValves(valves(id), opened, stepsLeft - 1, currentFlow)
          }.max
        else
          current.neighbours.flatMap { id =>
            Seq(
              currentFlow + traverseValves(valves(id), opened, stepsLeft - 1, currentFlow),
              (2 * currentFlow) + current.flow + traverseValves(
                valves(id),
                opened + current.id,
                stepsLeft - 2,
                currentFlow + current.flow
              )
            )
          }.max
      memory.put(cacheKey, result)
      result
    end if
  end traverseValves
  traverseValves(valves("AA"), valves.filter((_, v) => v.flow == 0).keySet, 30, 0)
end run01

def run02(in: Source): Long =
  ???

@main
def exercise16() =
  def source = Source.fromResource("input_16")
  print("Part 1: ")
  println(run01(source))
//print("Part 2: ")
//println(run02(source))
