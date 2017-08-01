lazy val commonSettings = Seq(
  scalaVersion := "2.12.3",
  organization := "ca.uwaterloo.flix",
  version := "0.1-SNAPSHOT"
)

lazy val dependencies = Seq(
  "com.github.scopt" %% "scopt" % "3.6.0",
  "org.scalatest" %% "scalatest" % "3.0.3" % Test,
  "org.parboiled" %% "parboiled" % "2.1.4",
  "org.json4s" %% "json4s-native" % "3.5.3",
  "org.ow2.asm" % "asm" % "5.2.0",
  "org.ow2.asm" % "asm-util" % "5.2.0",
  "org.ow2.asm" % "asm-analysis" % "5.2.0"
)

lazy val flix = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "flix",
    scalaSource in Compile := baseDirectory.value / "main" / "src",
    scalaSource in Test :=  baseDirectory.value / "main" / "test",
    unmanagedResourceDirectories in Compile += baseDirectory.value / "main" / "src" / "ca" / "uwaterloo" / "flix" / "runtime" / "debugger",
    scalacOptions ++= Seq("-feature", "-unchecked", "-deprecation"),
    libraryDependencies ++= dependencies,
    fork in Test :=  true,
    test := (testOnly in Test).toTask(" *.TestAll").value
  )
