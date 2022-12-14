package eu.derfniw.aoc2022.ex13

import scala.annotation.tailrec
import scala.collection.View.Unfold
import scala.io.Source
import scala.util.parsing.combinator.RegexParsers

enum NestedList:
  case N(n: Int)
  case L(l: List[NestedList])

object NestedList:

  given nestedListOrd: Ordering[NestedList] with
    def compare(left: NestedList, right: NestedList): Int =
      (left, right) match
        case (N(l), N(r)) => l.compare(r)
        case (l: N, r: L) => compare(L(List(l)), r)
        case (l: L, r: N) => compare(l, L(List(r)))
        case (L(ls), L(rs)) =>
          (ls, rs) match
            case (Nil, Nil) => 0
            case (Nil, _)   => -1
            case (_, Nil)   => 1
            case (l :: lRem, r :: rRem) =>
              val headCompare = compare(l, r)
              if headCompare == 0 then compare(L(lRem), L(rRem))
              else headCompare
  end nestedListOrd

end NestedList

object NestedListParser extends RegexParsers:
  import NestedList.*

  def number: Parser[N] = """\d+""".r ^^ { case num => N(num.toInt) }

  def listElement: Parser[NestedList] = number | nestedList

  def nestedList: Parser[NestedList] = "[" ~> repsep(listElement, ",") <~ "]" ^^ { case nls =>
    L(nls)
  }

  def parseLine(l: String): NestedList =
    parse(nestedList, l).get
end NestedListParser

def run01(in: Source): Int =
  import NestedList.nestedListOrd.mkOrderingOps

  val pairs = Unfold(in.getLines().toList) { lines =>
    lines match
      case left :: right :: remainder =>
        val newPair = (NestedListParser.parseLine(left), NestedListParser.parseLine(right))
        Some((newPair, remainder.drop(1)))
      case _ => None
  }.toList

  val orderResults = pairs.zipWithIndex.map { case ((left, right), idx) =>
    if left < right then idx + 1 else 0
  }

  orderResults.sum
end run01

def run02(in: Source): Int =
  import NestedList.nestedListOrd
  val divider1 = NestedListParser.parseLine("[[2]]")
  val divider2 = NestedListParser.parseLine("[[6]]")
  val packets = divider1 :: divider2 :: in
    .getLines()
    .filterNot(_.isBlank())
    .map(NestedListParser.parseLine)
    .toList
  val sortedPackets = packets.sorted
  (sortedPackets.indexOf(divider1) + 1) * (sortedPackets.indexOf(divider2) + 1)
end run02

@main
def exercise13() =
  def source = Source.fromResource("input_13")
  print("Part 1: ")
  println(run01(source))
  print("Part 2: ")
  println(run02(source))
