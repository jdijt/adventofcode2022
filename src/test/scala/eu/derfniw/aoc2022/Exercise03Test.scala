package eu.derfniw.aoc2022

import scala.io.Source

class Exercise03Test extends AocTestSuite:

  val sample = """|vJrwpWtwJgWrhcsFMMfFFhFp
                  |jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
                  |PmmdzqPrVvPwwTWBwg
                  |wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
                  |ttgJtRGJQctTZtZT
                  |CrZsJsPPZsGzwwsLwLmpwMDw""".stripMargin

  testSample("Part one on sample input", sample, ex03.run01, 157)
  testSample("Part two on sample input", sample, ex03.run02, 70)
end Exercise03Test
