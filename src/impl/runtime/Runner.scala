package impl.runtime

import impl.logic.{Symbol, Program}
import util.output.Solution

object Runner {
  def run(p: Program, hints: Map[Symbol.PredicateSymbol, Hint]): Unit = {
    val verifier = new Verifier(p)
    verifier.verify()

    val t = System.nanoTime()

    val solver = new Solver(p, hints)
    solver.solve()

    val e = System.nanoTime() - t

    Solution.print(solver)

    println("Time: " + (e / 1000000).toString + " ms")
  }
}
