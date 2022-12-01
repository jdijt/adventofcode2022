lazy val root = project
  .in(file("."))
  .enablePlugins(NativeImagePlugin)
  .settings(
    name         := "adventofcode2022",
    description  := "Jasper's advent of code stuff for 2022",
    version      := "0.1.0",
    scalaVersion := "3.2.1",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.1.1",
      "org.scalameta"          %% "munit"                    % "1.0.0-M7" % Test
    ),
    Compile / mainClass := Some("eu.derfniw.aoc2021.d13.run_13_2"),
    nativeImageOptions ++= Seq(
      "--static"
    )
  )
