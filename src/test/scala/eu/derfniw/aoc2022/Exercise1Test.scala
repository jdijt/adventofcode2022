package eu.derfniw.aoc2022

import scala.io.Source

class Exercise1Test extends AocTestSuite:

  val sample = """|1000
                  |2000
                  |3000
                  |
                  |4000
                  |
                  |5000
                  |6000
                  |
                  |7000
                  |8000
                  |9000
                  |
                  |10000""".stripMargin

  testStringSample("Part one on sample input", sample, ex01.run01, 24000)
  testStringSample("Part two on sample input", sample, ex01.run02, 45000)

end Exercise1Test
