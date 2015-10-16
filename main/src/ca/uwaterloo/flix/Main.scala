package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.runtime.Solver
import ca.uwaterloo.flix.util.{Shell, Options}

object Main {

  def main(args: Array[String]): Unit = {

    implicit val options = Options()

    val ast = Compiler.compile(args.filterNot(_.contains("-")).map(arg => Paths.get(arg)))

    if (ast.nonEmpty) {
      implicit val sCtx = Solver.SolverContext(ast.get)

      val solver = new Solver()
      solver.solve()

      if (args.exists(_.contains("-i"))) {
        val shell = new Shell()
        shell.startAndAwait()
      }
    }
  }

}
