package examples

import impl.logic.Program
import impl.runtime.{Solver, Verifier}
import util.output.StringFormat

object Runner {
  def run(p: Program): Unit = {
    val verifier = new Verifier(p)
    verifier.verify()

    val solver = new Solver(p)
    solver.solve()

    println(StringFormat.format(p))

    StringFormat.printSolution(solver)
  }
}
