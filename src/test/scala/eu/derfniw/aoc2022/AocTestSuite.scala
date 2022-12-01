package eu.derfniw.aoc2022

import scala.io.Source

abstract class AocTestSuite extends munit.BaseFunSuite:

  def testStringSample[A](
      name: String,
      input: String,
      toTest: Source => A,
      expected: A
  )(using loc: munit.Location) =
    testSourceSample(name, Source.fromString(input), toTest, expected)

  def testSourceSample[A](name: String, input: Source, toTest: Source => A, expected: A)(using
      loc: munit.Location
  ) =
    test(name) {
      assertEquals(toTest(input), expected)
    }
end AocTestSuite
