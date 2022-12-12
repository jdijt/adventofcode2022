package eu.derfniw.aoc2022

import eu.derfniw.aoc2022.Exercise11Test

import scala.io.Source
class Exercise11Test extends AocTestSuite:

  def sampleSource = Source.fromResource("input_11_test")

  testSample("Part one on sample input", sampleSource, ex11.run01, 10605)
  testSample("Part two on sample input", sampleSource, ex11.run02, 2713310158L)

end Exercise11Test
