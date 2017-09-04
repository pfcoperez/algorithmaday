name := "algorithmaday"

version := "1.0"

organization := "org.pfcoperez"

scalaVersion := "2.11.8"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.0",
  "org.scalacheck" %% "scalacheck" % "1.13.2" % "test",
  "org.typelevel" %% "cats" % "0.9.0",
  "com.storm-enroute" %% "scalameter" % "0.7"
)

testFrameworks += new TestFramework("org.scalameter.ScalaMeterFramework")

    
