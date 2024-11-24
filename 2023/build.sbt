Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.5.2"

name := "advent-of-code-2023"

libraryDependencies ++= List(
  "org.scala-lang.modules" %% "scala-collection-contrib"   % "0.3.0",
  "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
  "dev.zio"                %% "zio-prelude"                % "1.0.0-RC21",
  "dev.zio"                %% "zio-test"                   % "2.0.19" % Test,
  "dev.zio"                %% "zio-test-sbt"               % "2.0.19" % Test
)
