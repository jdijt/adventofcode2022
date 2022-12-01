package eu.derfniw.aoc2022

import scala.io.Source

class Exercise1Test extends munit.FunSuite:

  val sample = """|1000
                  |2000
                  |3000
                  |
                  |4000
                  |
                  |5000
                  |6000
                  |
                  |7000
                  |8000
                  |9000
                  |
                  |10000""".stripMargin

  test("Part 1 should work for sample input") {
    val in     = Source.fromString(sample)
    val result = ex01.run01(in)
    assertEquals(result, 24000)
  }

  test("Part 2 should work for sample input") {
    val in     = Source.fromString(sample)
    val result = ex01.run02(in)
    assertEquals(result, 45000)
  }
end Exercise1Test
