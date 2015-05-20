import sbt._

object build extends Build {
  // Ensures that both `main` and `macro` subprojects are built and tested
  lazy val root = Project("flix", file(".")).aggregate(flixMain, macroSub)

  lazy val flixMain = Project("main", file("main"))
  lazy val macroSub = Project("macro", file("macro")).dependsOn(flixMain)
}
