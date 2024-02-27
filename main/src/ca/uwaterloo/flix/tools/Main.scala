package ca.uwaterloo.flix.tools

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{Options, Result}

import java.nio.file.Paths

// temporary file
object Main {
  def main(argv: Array[String]): Unit = {
    implicit val flix: Flix = new Flix().setOptions(Options.TestWithLibAll).addFlix(Paths.get("main/src/ca/uwaterloo/flix/tools/Main.flix"))
    flix.options = Options.Default.copy(progress = true)

    val ast = flix.check().unsafeGet
    MutationTester.run(ast) match {
      case Result.Ok((all, killed, compilationFailed)) => println("all = " + all + " killed = " + killed + " compilationFailed = " + compilationFailed)
      case Result.Err(_) => println("something went wrong")
    }
  }
}
