import sbt._
import Keys._

object build extends Build {
  lazy val commonSettings = Seq(
    scalaVersion := "2.11.6",
    organization := "ca.uwaterloo.flix",
    version := "0.1-SNAPSHOT"
  )

  // Ensures that both subprojects are built and tested
  lazy val root = Project("flix-root", file("."))
    .aggregate(flixMain, macroSub)
    .settings(commonSettings: _*)
    .settings(
      // Don't publish the empty root project
      publish := {},
      publishLocal := {}
    )

  lazy val flixMain = Project("flix", file("main"))
    .settings(commonSettings: _*)

  lazy val macroSub = Project("flix-macros", file("macro"))
    .dependsOn(flixMain)
    .settings(commonSettings: _*)
}
