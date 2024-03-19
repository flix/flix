package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util.{Options, Result}

import java.nio.file.Paths

// temporary file
object Main {
  def main(argv: Array[String]): Unit = {
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("main/src/ca/uwaterloo/flix/tools/sources/IfElse.flix"))
//    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/print-a-colorful-message.flix"))
//    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/higher-order-functions.flix"))
//    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/larger-examples/Reachable.flix"))
    flix.setFormatter(AnsiTerminalFormatter)

    val formatter = flix.getFormatter
    import formatter._

    val ast = flix.check().unsafeGet
    MutationTester.run(ast) match {
      case Result.Ok(reporter) =>
        println()
        println(blue("Mutation testing:"))
        println(blue(s"All = ${reporter.all}, Killed = ${reporter.killed}, CompilationFailed = ${reporter.compilationFailed}."))
      case Result.Err(_) => println("Something went wrong")
    }
  }
}
