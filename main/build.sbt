scalaSource in Compile := baseDirectory.value / "src"

scalaSource in Test := baseDirectory.value / "test"

libraryDependencies += "com.chuusai" %% "shapeless" % "2.0.0"

libraryDependencies += "org.parboiled" %% "parboiled" % "2.1.0"

libraryDependencies += "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"

libraryDependencies += "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test"
