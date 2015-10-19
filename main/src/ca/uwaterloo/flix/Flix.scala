package ca.uwaterloo.flix

import java.nio.file.{Path, InvalidPathException, Files, Paths}

import ca.uwaterloo.flix.language.Compiler
import ca.uwaterloo.flix.runtime.{Model, Solver}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

object Flix {

  trait FlixError {

  }

  def solve(s: String): Unit = {
    if (isValidPath(s))
      Compiler.compile(Paths.get(s))
    else
      Compiler.compile(s)
  }

  def solve(paths: Traversable[Path]): Validation[Model, FlixError] = {
    val ast = Compiler.compile(paths)

    implicit val sCtx = Solver.SolverContext(ast.get)

    val solver = new Solver()
    val model = solver.solve()

    model.toSuccess
  }

  private def isValidPath(s: String): Boolean = try {
    val path = Paths.get(s)
    Files.exists(path) && Files.isRegularFile(path)
  } catch {
    case e: InvalidPathException => false
  }

}
