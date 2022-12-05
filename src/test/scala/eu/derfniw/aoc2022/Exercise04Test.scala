package eu.derfniw.aoc2022

import scala.io.Source

class Exercise04Test extends AocTestSuite:

  val sample = """|2-4,6-8
                  |2-3,4-5
                  |5-7,7-9
                  |2-8,3-7
                  |6-6,4-6
                  |2-6,4-8""".stripMargin

  testStringSample("Part one on sample input", sample, ex04.run01, 2)
  testStringSample("Part two on sample input", sample, ex04.run02, 4)
end Exercise04Test