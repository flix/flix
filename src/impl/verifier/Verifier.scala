package impl.verifier

import java.io.{File, PrintWriter}

import impl.logic._

import scala.collection.mutable

object Verifier {

  val Z3 = System.getProperty("Z3", "C:\\Program Files\\Microsoft Z3\\z3-4.3.0-x64\\bin\\z3.exe")

  // A map from types to type names.
  val types = Map.empty[Type, String]

  def lookupType(typ: Type): String = ???

  /**
   * Verifies that the program is safe.
   */
  def verify(program: Program): Unit = {
    for (decl <- program.declarations) {
      decl match {
        case Declaration.DeclareLeq(t, Type.Function(typ1, typ2)) =>

          val typDecl = foo(Type.Tuple3(Type.Int, Type.Tuple2(Type.Bool, Type.Tuple2(Type.Bool, Type.Bool)), Type.Bool))

        case _ => // TODO Verifier
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

    case Term.Tagged(x, t1, _) => SmtExp.Lst(List(SmtExp.Literal(x.s), compileTerm(t1)))
    case Term.Tuple2(t1, t2) => SmtExp.Lst(List(SmtExp.Literal("Tuple2"), compileTerm(t1), compileTerm(t2)))
    case Term.Tuple3(t1, t2, t3) => SmtExp.Lst(List(SmtExp.Literal("Tuple3"), compileTerm(t1), compileTerm(t2), compileTerm(t3)))
    case Term.Tuple4(t1, t2, t3, t4) => SmtExp.Lst(List(SmtExp.Literal("Tuple4"), compileTerm(t1), compileTerm(t2), compileTerm(t3), compileTerm(t4)))
    case Term.Tuple5(t1, t2, t3, t4, t5) => SmtExp.Lst(List(SmtExp.Literal("Tuple5"), compileTerm(t1), compileTerm(t2), compileTerm(t3), compileTerm(t4), compileTerm(t5)))

  }


  private var Counter: Int = 0

  private def freshTypeName: String = {
    Counter += 1
    "t" + Counter
  }

//  (declare-datatypes (T1 T2) ((Pair (mk-pair (first T1) (second T2)))))
//  (declare-datatypes (T1) ((Tag (mk-tag (first T1)))))
//  (define-fun f ((x (Pair Int Int))) Int (first x))
//  (declare-const p1 (Pair Int (Pair Bool Bool)))
//  (declare-const p2 (Pair Int (Pair Bool Bool)))
//  (assert (not (= p1 p2)))
//  (check-sat)
//  (get-model)

  //  (declare-datatypes () ((Tuple2_Sort (Tuple2 (x Int) (y Int)))))
  //  (declare-const z Tuple2_Sort)
  //  (assert (= z (Tuple2 1 2)))
  //  (check-sat)
  //  (get-model)

//  (declare-datatypes () ((Pair (mk-pair (first Int) (second Int)))))
//  (declare-datatypes () ((Flaf (mk-flaf (fst Pair)))))
//  (declare-const p1 Flaf)
//  (declare-const p2 Flaf)
//  (assert (not (= p1 p2)))
//  (check-sat)
//  (get-model)

  // (declare-datatypes () ((SortName (ConstructorName (FieldName Type)))))

  def foo(typ: Type): List[SmtExp] = {
    val types = mutable.Map.empty[Type, String]
    val result = mutable.ListBuffer.empty[SmtExp]
    def visit(typ: Type): String = typ match {
      case Type.Unit => "unit"
      case Type.Bool => "bool"
      case Type.Int => "int"
      case Type.Str => "str"

      case Type.Tuple2(typ1, typ2) =>
        val (sortName, fieldSort1, fieldSort2) = (freshTypeName, visit(typ1), visit(typ2))
        types += typ -> sortName
        result += SmtExp.Lst(List(SmtExp.Literal("declare-type"),  SmtExp.Lst(Nil), SmtExp.Literal(sortName),
          SmtExp.Lst(List(SmtExp.Literal(freshTypeName), SmtExp.Literal(fieldSort1))),
          SmtExp.Lst(List(SmtExp.Literal(freshTypeName), SmtExp.Literal(fieldSort2)))))
        sortName
      case Type.Tuple3(typ1, typ2, typ3) =>
        val (sortName, fieldSort1, fieldSort2, fieldSort3) =  (freshTypeName, visit(typ1), visit(typ2), visit(typ3))
        types += typ -> sortName
        result += SmtExp.Lst(List(SmtExp.Literal("declare-type"), SmtExp.Lst(Nil), SmtExp.Literal(sortName), SmtExp.Literal(fieldSort1), SmtExp.Literal(fieldSort2), SmtExp.Literal(fieldSort3)))
        sortName

    }
    visit(typ)

    result.toList
  }


}
