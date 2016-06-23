lazy val commonSettings = Seq(
  scalaVersion := "2.11.7",
  organization := "ca.uwaterloo.flix",
  version := "0.1-SNAPSHOT"
)

lazy val flix = (project in file("."))
  .settings(commonSettings: _*)
  .settings(
    name := "flix",
    scalaSource in Compile := baseDirectory.value / "main" / "src",
    scalaSource in Test :=  baseDirectory.value / "main" / "test",
    unmanagedResourceDirectories in Compile += baseDirectory.value / "main" / "src" / "ca" / "uwaterloo" / "flix" / "runtime" / "debugger",
    scalacOptions ++= Seq("-unchecked", "-deprecation")
  )
