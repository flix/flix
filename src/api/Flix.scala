package api

import impl.ast._
import impl.logic.Value
import impl.runtime._

class Flix {
  def +=(s: String) = Parser.parse(s).foreach(compiler.compileDeclaration(_))

  def +=(functionMapping: (String, Value=>Value)) =
    compiler.addScalaFunction(functionMapping._1, functionMapping._2)

  def solve(options: Options = Options(propagation = Propagation.Full, simplify = Simplify.Enable)) = {
    solver = new Solver(compiler.outputProgram, options)
    solver.solve
  }

  def print() = solver.print

  def relation(name: String): List[Value] = solver.datastore.dumpPred(name)

  private val compiler = new Compiler
  private var solver: Solver = null
}

object FlixImplicits {
  implicit def convertString(s: String) = Value.Str(s)
}

