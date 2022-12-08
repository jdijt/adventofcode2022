package eu.derfniw.aoc2022

class Exercise08Test extends AocTestSuite:

  val sample = """|30373
                  |25512
                  |65332
                  |33549
                  |35390""".stripMargin

  testStringSample("Part one on sample input", sample, ex08.run01, 21)
  testStringSample("Part two on sample input", sample, ex08.run02, 8)
