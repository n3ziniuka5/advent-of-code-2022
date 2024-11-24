Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

name := "advent-of-code-2022"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-collection-contrib"   % "0.3.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "dev.zio"                %% "zio-test"                   % "2.0.4" % Test,
  "dev.zio"                %% "zio-test-sbt"               % "2.0.4" % Test
)
