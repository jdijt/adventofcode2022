package eu.derfniw.aoc2022

import scala.io.Source

class Exercise10Test extends AocTestSuite:

  def sampleSource = Source.fromResource("input_10_test")

  testSample("Part one on sample input", sampleSource, ex10.run01, 13140)

  val partTwoExpected =
    """|##..##..##..##..##..##..##..##..##..##..
       |###...###...###...###...###...###...###.
       |####....####....####....####....####....
       |#####.....#####.....#####.....#####.....
       |######......######......######......####
       |#######.......#######.......#######.....""".stripMargin

  testSample("Part two on sample input", sampleSource, ex10.run02, partTwoExpected)
end Exercise10Test
