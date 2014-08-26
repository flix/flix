package impl.verifier

import java.io.{File, PrintWriter}

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import impl.runtime.Unification
import syntax.Symbols._

object Verifier {

  val Z3 = System.getProperty("Z3", "C:\\Program Files\\Microsoft Z3\\z3-4.3.0-x64\\bin\\z3.exe")

  /**
   * Verifies that the program is safe.
   */
  def verify(program: Program): Unit = {

  }

  /**
   * Emit verifications conditions.
   */
  private def emitVerificationConditions(): Unit = {
//    /**
//     * Verification conditions for the lattice ordering: Leq.
//     */
//    run(lattice, "Leq is reflexivity", LatticeLeq.reflexivity(lattice.name, lattice.leq))
//    run(lattice, "Leq is antisymmetric", LatticeLeq.antiSymmetri(lattice.name, lattice.leq))
//    run(lattice, "Leq is transitive", LatticeLeq.transitivity(lattice.name, lattice.leq))
//    run(lattice, "Leq has a bottom element", LatticeLeq.leastElement(lattice.name, lattice.bot, lattice.leq))
//
//    /**
//     * Verification conditions for the lattice least-upper-bound: Lub.
//     */
//    run(lattice, "Lub is an upper bound", LatticeLub.upperBound(lattice.name, lattice.leq, lattice.lub))
//    run(lattice, "Lub is a least upper bound", LatticeLub.leastUpperBound(lattice.name, lattice.leq, lattice.lub))
//
//    /**
//     * Verification conditions for the lattice height.
//     */
//    run(lattice, "The height function is strictly decreasing", LatticeHeight.strictlyDecreasing(lattice.name, lattice.height, lattice.leq))
//    run(lattice, "The height function is always non-negative", LatticeHeight.nonNegative(lattice.name, lattice.height))
//
//    for (s <- lattice.funcs) {
//      run(lattice, s.fmt + " is monotone", Transfer.isMonotone2(lattice.name, s, lattice.leq))
//    }
  }

  /**
   * Run Z3 on the given input.
   */
  def run(name: String, s: String): Unit = {
    // Create a temporary file and store the lattice declarations and constraints.
    val tmpFile = File.createTempFile("z3-constraint", ".txt");
    val writer = new PrintWriter(tmpFile)
    writer.println(s)
    writer.close()

    // Run z3
    val process = Runtime.getRuntime.exec(Array(Z3, "/smt2", tmpFile.getAbsolutePath))
    val output = scala.io.Source.fromInputStream(process.getInputStream).getLines().mkString("\n")

    if (output == "sat") {
      println(s"$name: Yes.")
    } else {
      println()
      println(s"$name: NO!!!!!!!!")
      println(s"Constraint File: $tmpFile")
      println(s"Z3 Output:")
      println(output)
      println()
    }
  }
}
