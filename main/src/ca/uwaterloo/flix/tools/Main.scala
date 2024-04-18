package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.{BootstrapError, Flix}
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.runtime.CompilationResult
import ca.uwaterloo.flix.util.Formatter.AnsiTerminalFormatter
import ca.uwaterloo.flix.util.collection.Chain
import ca.uwaterloo.flix.util.{Options, Result, Validation}

import java.nio.file.Paths

// temporary file
object Main {
  def main(argv: Array[String]): Unit = {
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll)
//      .addFlix(Paths.get("main/src/ca/uwaterloo/flix/tools/personal/Int32.flix"))
      .addFlix(Paths.get("examples/an-interpreter-for-a-trivial-expression-language.flix")) // good example

    flix.setFormatter(AnsiTerminalFormatter)

    flix.compile().toHardResult match {
      case Result.Ok(r: CompilationResult) =>
        MutationTester.run(flix.getTyperAst, r) match {
          case Result.Ok(_) =>
          case Result.Err(e) => println(e)
        }
      case Result.Err(errors: Chain[CompilationMessage]) =>
        Validation.toHardFailure(BootstrapError.GeneralError(flix.mkMessages(errors)))
    }
  }
}
