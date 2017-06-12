
name := "Sentiment Analysis Project"

version := "1.0"

scalaVersion := "2.11.7"

scalacOptions ++= Seq("-unchecked", "-deprecation")

initialCommands in console := 
  """
  import Main._
  """

libraryDependencies += "org.apache.spark" %% "spark-core" % "2.1.0"

libraryDependencies += "org.apache.spark" %% "spark-mllib" % "2.1.0"

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "3.0.1" % "test"
