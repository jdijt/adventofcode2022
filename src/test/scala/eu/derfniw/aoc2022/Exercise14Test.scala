package eu.derfniw.aoc2022

class Exercise14Test extends AocTestSuite:

  def sample = """|498,4 -> 498,6 -> 496,6
                  |503,4 -> 502,4 -> 502,9 -> 494,9""".stripMargin

  testSample("Part one on sample input", sample, ex14.run01, 24)
  testSample("Part two on sample input", sample, ex14.run02, 93)

end Exercise14Test
