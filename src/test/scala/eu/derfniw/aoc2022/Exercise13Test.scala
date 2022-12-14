package eu.derfniw.aoc2022

class Exercise13Test extends AocTestSuite:

  def sample = """|[1,1,3,1,1]
                  |[1,1,5,1,1]
                  |
                  |[[1],[2,3,4]]
                  |[[1],4]
                  |
                  |[9]
                  |[[8,7,6]]
                  |
                  |[[4,4],4,4]
                  |[[4,4],4,4,4]
                  |
                  |[7,7,7,7]
                  |[7,7,7]
                  |
                  |[]
                  |[3]
                  |
                  |[[[]]]
                  |[[]]
                  |
                  |[1,[2,[3,[4,[5,6,7]]]],8,9]
                  |[1,[2,[3,[4,[5,6,0]]]],8,9]
                  |""".stripMargin

  testSample("Part one on sample input", sample, ex13.run01, 13)
  testSample("Part two on sample input", sample, ex13.run02, 140)

end Exercise13Test
