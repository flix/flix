lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "ca.uwaterloo.flix",
  version := "0.1-SNAPSHOT"
)

lazy val dependencies = Seq(
  "org.scalatest" %% "scalatest" % "2.2.4" % Test,
  "org.parboiled" %% "parboiled" % "2.1.0",
  "org.json4s" %% "json4s-native" % "3.2.11"
)

lazy val root = (project in file("main"))
  .settings(commonSettings: _*)
  .settings(
    name := "flix",
    scalaSource in Compile := baseDirectory.value / "src",
    scalaSource in Test :=  baseDirectory.value / "test",
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    libraryDependencies ++= dependencies
  )
