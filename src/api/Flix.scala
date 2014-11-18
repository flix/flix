package api

import impl.ast._
import impl.runtime._

class Flix {
  def +=(s: String) = Parser.parse(s).foreach(compiler.compileDeclaration(_))
  def solve(options: Options = Options(propagation = Propagation.Full, simplify = Simplify.Enable)) = {
    solver = new Solver(compiler.outputProgram, options)
    solver.solve
  }
  def print() = solver.print

  val compiler = new Compiler
  var solver: Solver = null
}
