import sbt._

object Dependencies {
  lazy val scalaTest 	= "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val scalaMeter = "com.storm-enroute" %% "scalameter" % "0.17"
  lazy val snakeYaml  = "org.yaml" % "snakeyaml" % "1.23"
  lazy val akkaActor  = "com.typesafe.akka" %% "akka-actor" % "2.5.23"
  def all = Seq(
    scalaTest,
    scalaMeter,
    snakeYaml,
    akkaActor
  )
}

