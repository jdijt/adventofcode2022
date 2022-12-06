package eu.derfniw.aoc2022

import scala.io.Source

class Exercise06Test extends AocTestSuite:

  val samplesResponses: Seq[(String, Int, Int)] = Seq(
    ("mjqjpqmgbljsphdztnvjfqwrcgsmlb", 7, 19),
    ("bvwbjplbgvbhsrlpgdmjqwftvncz", 5, 23),
    ("nppdvjthqldpwncqszvftbrmjlhg", 6, 23),
    ("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg", 10, 29),
    ("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw", 11, 26)
  )

  samplesResponses.zipWithIndex.foreach { case ((in, out, _), idx) =>
    testStringSample(s"Part one on sample ${idx + 1}", in, ex06.run01, out)
  }

  samplesResponses.zipWithIndex.foreach { case ((in, _, out), idx) =>
    testStringSample(s"Part two on sample ${idx + 1}", in, ex06.run02, out)
  }
end Exercise06Test
