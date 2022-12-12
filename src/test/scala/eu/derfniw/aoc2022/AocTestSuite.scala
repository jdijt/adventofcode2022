package eu.derfniw.aoc2022

import scala.io.Source

abstract class AocTestSuite extends munit.BaseFunSuite:

  def testSample[A](
      name: String,
      input: String,
      toTest: Source => A,
      expected: A
  )(using loc: munit.Location): Unit =
    testSample(name, Source.fromString(input), toTest, expected)

  def testSample[A](name: String, input: Source, toTest: Source => A, expected: A)(using
      loc: munit.Location
  ): Unit =
    test(name) {
      assertEquals(toTest(input), expected)
    }
end AocTestSuite
