scalaVersion := "2.11.6"

scalacOptions += "-deprecation"

scalacOptions += "-feature"

libraryDependencies <+= scalaVersion("org.scala-lang" % "scala-compiler" % _)
