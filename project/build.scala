import sbt._
import Keys._

object build extends Build {
  lazy val flix = Project("flix", file(".")) dependsOn(macroSub)
  lazy val macroSub = Project("macro", file("macro"))
}
