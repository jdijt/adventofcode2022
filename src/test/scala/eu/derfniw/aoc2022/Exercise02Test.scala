package eu.derfniw.aoc2022

import scala.io.Source

class Exercise02Test extends AocTestSuite:

  val sample = """|A Y
                  |B X
                  |C Z""".stripMargin

  testStringSample("Part one on sample input", sample, ex02.run01, 15)
  testStringSample("Part two on sample input", sample, ex02.run02, 12)
