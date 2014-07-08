package impl.runtime

import impl.logic.Program
import util.output.Solution

object Runner {
  def run(p: Program): Unit = {
    val verifier = new Verifier(p)
    verifier.verify()

    val solver = new Solver(p)
    solver.solve()

    Solution.print(solver)
  }
}
