package eu.derfniw.aoc2022

class Exercise09Test extends AocTestSuite:

  val sample = """|R 4
                  |U 4
                  |L 3
                  |D 1
                  |R 4
                  |D 1
                  |L 5
                  |R 2""".stripMargin
  val sample2 = """|R 5
                   |U 8
                   |L 8
                   |D 3
                   |R 17
                   |D 10
                   |L 25
                   |U 20""".stripMargin

  testStringSample("Part one on sample input", sample, ex09.run01, 13)
  testStringSample("Part two on sample input", sample, ex09.run02, 1)
  testStringSample("Part two on sample input 2", sample2, ex09.run02, 36)
end Exercise09Test
