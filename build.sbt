ThisBuild / scalaVersion     := "2.12.8"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.blee"
ThisBuild / organizationName := "blee"

lazy val root = (project in file("."))
  .settings(
    name := "scala-ray-tracer",
    libraryDependencies ++= Dependencies.all map(_ withSources())
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
