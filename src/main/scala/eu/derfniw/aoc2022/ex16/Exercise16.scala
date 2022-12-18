package eu.derfniw.aoc2022.ex16

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
  def traverseValves(
      current: Valve,
      opened: Set[String],
      totalFlow: Int,
      currentFlow: Int,
      stepsLeft: Int
  ): Int =
    if stepsLeft <= 0 then totalFlow
    else if valves.keySet == opened then totalFlow + stepsLeft * currentFlow
    else
      val results =
        if opened(current.id) then
          current.neighbours.map { id =>
            traverseValves(valves(id), opened, totalFlow + currentFlow, currentFlow, stepsLeft - 1)
          }
        else
          current.neighbours.flatMap { id =>
            Seq(
              traverseValves(
                valves(id),
                opened,
                totalFlow + currentFlow,
                currentFlow,
                stepsLeft - 1
              ),
              traverseValves(
                valves(id),
                opened + current.id,
                totalFlow + currentFlow + current.flow,
                currentFlow,
                stepsLeft - 2
              )
            )
          }
      results.max
  traverseValves(valves("AA"), valves.filter((_, v) => v.flow == 0).keySet, 0, 0, 30)
end run01

def run02(in: Source): Long =
  ???

@main
def exercise16() =
  def source = Source.fromResource("input_15")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
