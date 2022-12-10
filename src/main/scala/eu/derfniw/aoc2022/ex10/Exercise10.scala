package eu.derfniw.aoc2022.ex10

import scala.io.Source

case class ProgramState(xRegister: Int):
  def applyInstruction(instr: String): (ProgramState, Seq[Int]) = instr match
    case s"addx $amount" =>
      val newState = ProgramState(xRegister + amount.toInt)
      (newState, Seq(xRegister, newState.xRegister))
    case "noop" => (this, Seq(xRegister))

def run01(in: Source) =
  val (_, values) =
    in.getLines().foldLeft((ProgramState(1), IndexedSeq[Int](1))) {
      case ((state, cycleLog), instr) =>
        val (newState, newLogValues) = state.applyInstruction(instr)
        (newState, cycleLog ++ newLogValues)
    }
  val signals = Seq(
    (values(19) * 20),
    (values(59) * 60),
    (values(99) * 100),
    (values(139) * 140),
    (values(179) * 180),
    (values(219) * 220)
  )
  signals.sum
end run01

def run02(in: Source) =
  val (_, signals) =
    in.getLines().foldLeft((ProgramState(1), IndexedSeq[Int](1))) { case ((state, cycleLog), instr) =>
      val (newState, newLogValues) = state.applyInstruction(instr)
      (newState, cycleLog ++ newLogValues)
    }
  signals
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
