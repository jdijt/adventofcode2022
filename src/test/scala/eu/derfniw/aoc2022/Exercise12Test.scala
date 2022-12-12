package eu.derfniw.aoc2022

import scala.io.Source

class Exercise12Test extends AocTestSuite:

  def sample = """|Sabqponm
                  |abcryxxl
                  |accszExk
                  |acctuvwj
                  |abdefghi""".stripMargin

  testSample("Part one on sample input", sample, ex12.run01, 31)
  testSample("Part two on sample input", sample, ex12.run02, 29)

end Exercise12Test
