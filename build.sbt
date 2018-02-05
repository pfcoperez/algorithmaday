name := "algorithmaday"

version := "1.0"

organization := "org.pfcoperez"

val projectScalaVersion = "2.11.8"

scalaVersion := projectScalaVersion

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.typelevel" %% "cats-core" % "1.0.1",
  "com.storm-enroute" %% "scalameter" % "0.7"
)

scalaVersion in ThisBuild := projectScalaVersion

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

    
