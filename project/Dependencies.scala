import sbt._

object Dependencies {
  lazy val snakeYaml    = "org.yaml"            %   "snakeyaml"   % "1.23"
  lazy val fastparse    = "com.lihaoyi"         %%  "fastparse"   % "2.1.3"
  lazy val scalaMeter   = "com.storm-enroute"   %%  "scalameter"  % "0.17"
  lazy val akkaActor    = "com.typesafe.akka"   %%  "akka-actor"  % "2.5.23"
  lazy val scalaTest 	  = "org.scalatest"       %%  "scalatest"   % "3.0.8" % Test

  def all = Seq(
    snakeYaml,
    fastparse,
    scalaMeter,
    akkaActor,
    scalaTest
  )
}

