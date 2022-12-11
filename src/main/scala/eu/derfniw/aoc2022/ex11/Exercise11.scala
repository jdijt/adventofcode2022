package eu.derfniw.aoc2022.ex11

import scala.collection.Map
import scala.collection.View.Unfold
import scala.collection.immutable.Queue
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

case class Monkey(
    id: Int,
    items: Queue[Long],
    worryOp: Long => Long,
    targetModulus: Long,
    targetIfMod0: Int,
    targetElse: Int
) extends Ordered[Monkey]:

  def compare(that: Monkey): Int = this.id.compare(that.id)

  def catchItem(item: Long) = this.copy(items = items.enqueue(item))

  def performCycle1(monkeys: Map[Int, Monkey], commonDivisor: Long): Map[Int, Monkey] =
    val newWorryValues = items.map(worryOp).map(_ / 3).map(_ % commonDivisor)
    val newMonkeys = newWorryValues.foldLeft(monkeys) { (state, item) =>
      val target = state(if item % targetModulus == 0 then targetIfMod0 else targetElse)
      state + (target.id -> target.catchItem(item))
    }
    newMonkeys + (id -> this.copy(items = Queue()))

  def performCycle2(monkeys: Map[Int, Monkey], commonDivisor: Long): Map[Int, Monkey] =
    val newWorryValues = items.map(worryOp).map(_ % commonDivisor)
    val newMonkeys = newWorryValues.foldLeft(monkeys) { (state, item) =>
      val target = state(if item % targetModulus == 0 then targetIfMod0 else targetElse)
      state + (target.id -> target.catchItem(item))
    }
    newMonkeys + (id -> this.copy(items = Queue()))
end Monkey

object MonkeyParser extends RegexParsers:
  override def skipWhitespace: Boolean = false

  def number: Parser[Int] = """\d+""".r ^^ { case n => n.toInt }

  def monkeyId: Parser[Int] = "Monkey " ~> number <~ ":\n"

  def startingItems: Parser[List[Int]] = "  Starting items: " ~> repsep(number, ", ") <~ "\n"

  def worryOperation: Parser[Long => Long] =
    def exp: Parser[Long => Long] = "old * old" ^^ { case _ => (n => n * n) }
    def fixed: Parser[Long => Long] = "old " ~ ("*" | "+") ~ " " ~ number ^^ {
      case _ ~ "+" ~ _ ~ value => (n => n + value)
      case _ ~ "*" ~ _ ~ value => (n => n * value)

    }
    "  Operation: new = " ~> (exp | fixed) <~ "\n"

  def targetOperations: Parser[(Long, Int, Int)] =
    "  Test: divisible by " ~ number
      ~ "\n    If true: throw to monkey " ~ number
      ~ "\n    If false: throw to monkey " ~ number <~ "\n".? ^^ {
        case _ ~ mod ~ _ ~ ifTrue ~ _ ~ ifFalse => (mod, ifTrue, ifFalse)
      }

  def monkeyParser: Parser[Monkey] =
    monkeyId ~ startingItems ~ worryOperation ~ targetOperations ^^ {
      case id ~ items ~ worryOp ~ (targetMod, ifTrue, ifFalse) =>
        Monkey(id, Queue(items.map(_.toLong)*), worryOp, targetMod, ifTrue, ifFalse)
    }

  def monkeys: Parser[Map[Int, Monkey]] = repsep(monkeyParser, "\n") ^^ { case monkeys =>
    Map(monkeys.map(m => (m.id -> m))*)
  }

  def parseMonkeys(input: Source): Map[Int, Monkey] =
    val result = parse(monkeys, input.mkString)
    result.get
end MonkeyParser

def run(
    monkeys: Map[Int, Monkey],
    iterations: Int,
    cycleFunction: (Map[Int, Monkey], Monkey, Long) => Map[Int, Monkey]
): Long =
  val commonDivisor = monkeys.values.map(_.targetModulus).product
  val initialCounts = (0 until monkeys.size).map(i => (i -> 0L)).toMap
  val inspectionCounts = (0 until iterations * monkeys.size)
    .foldLeft((monkeys, initialCounts)) { case ((oldMonkeys, oldCounts), round) =>
      val currentMonkey = oldMonkeys((round % monkeys.size))
      val newCounts =
        oldCounts + (currentMonkey.id -> (oldCounts(currentMonkey.id) + currentMonkey.items.length))
      val newMonkeys = cycleFunction(oldMonkeys, currentMonkey, commonDivisor)
      (newMonkeys, newCounts)
    }
    ._2
    .values
    .toSeq

  inspectionCounts.sorted.reverse.take(2).product
end run

def run01(in: Source): Long =
  val monkeys = MonkeyParser.parseMonkeys(in)
  run(monkeys, 20, (state, monkey, commonDivisor) => monkey.performCycle1(state, commonDivisor))

def run02(in: Source): Long =
  val monkeys = MonkeyParser.parseMonkeys(in)
  run(monkeys, 10000, (state, monkey, commonDivisor) => monkey.performCycle2(state, commonDivisor))

@main
def exercise10() =
  def source = Source.fromResource("input_11")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
