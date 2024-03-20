package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util.{Options, Result}

import java.nio.file.Paths

// temporary file
object Main {
  def main(argv: Array[String]): Unit = {
//    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("main/src/ca/uwaterloo/flix/tools/tempdir/Int32.flix"))
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/larger-examples/Reachable.flix"))
    flix.setFormatter(AnsiTerminalFormatter)

    val ast = flix.check().unsafeGet
    MutationTester.run(ast) match {
      case Result.Ok(reporter) =>
        println()
        println(reporter.printStats())
      case Result.Err(_) => println("Something went wrong")
    }
  }
}
