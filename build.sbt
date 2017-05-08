name := """hello-scala"""

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq( "org.scalatest" %% "scalatest" % "2.2.4" % "test",
"org.json4s" %% "json4s-jackson" % "3.3.0",
"org.json4s" %% "json4s-ext" % "3.3.0")


fork in run := false