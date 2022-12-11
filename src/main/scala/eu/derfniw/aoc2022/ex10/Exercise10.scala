package eu.derfniw.aoc2022.ex10

import scala.io.Source

case class ProgramState(xRegister: Int):
  def applyInstruction(instr: String): (ProgramState, Seq[Int]) = instr match
    case s"addx $amount" =>
      val newState = ProgramState(xRegister + amount.toInt)
      (newState, Seq(xRegister, newState.xRegister))
    case "noop" => (this, Seq(xRegister))

def executeProgram(in: Source): IndexedSeq[Int] =
  in.getLines()
    .foldLeft((ProgramState(1), IndexedSeq[Int](1))) { case ((state, cycleLog), instr) =>
      val (newState, newLogValues) = state.applyInstruction(instr)
      (newState, cycleLog ++ newLogValues)
    }
    ._2

def run01(in: Source) =
  val values = executeProgram(in)
  Seq(20, 60, 100, 140, 180, 220).map(idx => values(idx - 1) * idx).sum

def run02(in: Source) =
  executeProgram(in)
    .sliding(40, 40)
    .filter(_.length == 40)
    .map { lineValues =>
      lineValues.zipWithIndex
        .map((value, idx) => if math.abs(value - idx) <= 1 then '#' else '.')
        .mkString
    }
    .mkString("\n")
end run02

@main
def exercise10() =
  def source = Source.fromResource("input_10")
  print("Part 1: ")
  println(run01(source))
  println("Part 2: ")
  println(run02(source))
