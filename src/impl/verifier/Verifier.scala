package impl.verifier

import java.io.{File, PrintWriter}

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._

object Verifier {

  val Z3 = System.getProperty("Z3", "C:\\Program Files\\Microsoft Z3\\z3-4.3.0-x64\\bin\\z3.exe")

  // A map from types to type names.
  val types = Map.empty[Type, String]

  def lookupType(typ: Type): String = types.get(typ) match {
    case None => ??? // TODO: Gen fresh name
    case Some(s) => s
  }

  /**
   * Verifies that the program is safe.
   */
  def verify(program: Program): Unit = {
    for (decl <- program.declarations) {
      decl match {
        case Declaration.DeclareLeq(t, typ) =>
          LatticeLeq.reflexivity(???, ???)
        case _ => // TODO
      }
    }
  }

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



  def compileTerm(t: Term): SmtExp = t match {
    case Term.Unit => SmtExp.Literal("unit")
    case Term.Bool(b) => SmtExp.Literal(b.toString)
    case Term.Int(i) => SmtExp.Literal(i.toString)
    case Term.Str(s) => SmtExp.Literal(s.hashCode.toString) // TODO: Need a better way to map string constants to integers.

    case Term.Var(x) => ???

    case Term.Tagged(x, t1, _) =>             List(SmtExp.Literal(x.s), compileTerm(t1))
    case Term.Tuple2(t1, t2) =>               List(SmtExp.Literal("Tuple2"), compileTerm(t1), compileTerm(t2))
    case Term.Tuple3(t1, t2, t3) =>           List(SmtExp.Literal("Tuple3"), compileTerm(t1), compileTerm(t2), compileTerm(t3))
    case Term.Tuple4(t1, t2, t3, t4) =>       List(SmtExp.Literal("Tuple4"), compileTerm(t1), compileTerm(t2), compileTerm(t3), compileTerm(t4))
    case Term.Tuple5(t1, t2, t3, t4, t5) =>   List(SmtExp.Literal("Tuple5"), compileTerm(t1), compileTerm(t2), compileTerm(t3), compileTerm(t4), compileTerm(t5))

  }

  //  (declare-datatypes () ((Tuple2_Sort (Tuple2 (x Int) (y Int)))))
  //  (declare-const z Tuple2_Sort)
  //  (assert (= z (Tuple2 1 2)))
  //  (check-sat)
  //  (get-model)

  def compileType(typ: Type): SmtExp = typ match {
    case Type.Unit => SmtExp.Literal("Unit")
    case Type.Bool => SmtExp.Literal("Bool")
    case Type.Int => SmtExp.Literal("Int")
    case Type.Str => SmtExp.Literal("Int") // NB: Not a typo. Strings are simple constants.

    case Type.Tagged(s, typ1) => ???
    case Type.Tuple2(typ1, typ2) => List(SmtExp.Literal("declare-datatypes"), SmtExp.Literal("()"), SmtExp.Lst(List()))
    case Type.Tuple3(typ1, typ2, typ3) => ???
    case Type.Tuple4(typ1, typ2, typ3, typ4) => ???
    case Type.Tuple5(typ1, typ2, typ3, typ4, typ5) => ???
  }

  import scala.language.implicitConversions
  private implicit def lift(xs: List[SmtExp]): SmtExp = SmtExp.Lst(xs)

}
