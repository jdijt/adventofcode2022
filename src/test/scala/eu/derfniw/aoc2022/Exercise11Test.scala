package eu.derfniw.aoc2022

import scala.io.Source

class Exercise11Test extends AocTestSuite:

  def sampleSource = Source.fromResource("input_11_test")

  testSourceSample("Part one on sample input", sampleSource, ex11.run01, 10605)
  testSourceSample("Part two on sample input", sampleSource, ex11.run02, 2713310158L)

end Exercise11Test
