package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util.{Options, Result}

import java.nio.file.Paths

// temporary file
object Main {
  def main(argv: Array[String]): Unit = {
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("main/src/ca/uwaterloo/flix/tools/personal/Simple.flix"))
    //    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/larger-examples/Reachable.flix"))
    //    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/larger-examples/datalog/ford-fulkerson.flix"))
    //    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("examples/an-interpreter-for-a-trivial-expression-language.flix")) // good example

    // todo: reporting nor works for 'NoFormatter'
    flix.setFormatter(AnsiTerminalFormatter)

    val ast = flix.check().unsafeGet
    MutationTester.run(ast) match {
      case Result.Ok(_) =>
      case Result.Err(e) => println(e)
    }
  }
}
