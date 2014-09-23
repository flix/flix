package impl.verifier

import java.io.{File, PrintWriter}

import impl.logic._
import SmtExp._

import scala.collection.mutable

object Verifier {

  val Z3 = System.getProperty("Z3", "C:\\Program Files\\Microsoft Z3\\z3-4.3.0-x64\\bin\\z3.exe")

  class TypeCache {
    var counter = 0
    val cache = mutable.Map.empty[Type, String]

    def getName(t: Type): String = cache.get(t) match {
      case Some(s) => s
      case None =>
        counter += 1
        cache += (t -> ("t" + counter))
        "t" + counter
    }
  }

  val types = new TypeCache

  /**
   * Verifies that the program is safe.
   */
  def verify(program: Program): Unit = {
    for (declaration <- program.declarations) {
      declaration match {
        case Declaration.DeclareBot(t, typ) =>
        case Declaration.DeclareLeq(t, typ) =>
        case Declaration.DeclareLub(t, typ) => compileFunction(t)
        case Declaration.DeclareHeight(t, typ) =>
      }
    }

    for (constraint <- program.constraints) {
      // TODO: Find functions...
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

  /**
   * Returns the given term `t` as an SMT-LIB function expression.
   */
  def compileFunction(t: Term.Abs): SmtExp = {
    def findArgs(t: Term): List[(Symbol.VariableSymbol, Type)] = t match {
      case Term.Abs(x, typ, t1) => (x, typ) :: findArgs(t1)
      case _ => Nil
    }
    def findBody(t: Term): Term = t match {
      case Term.Abs(_, _, t1) => findBody(t1)
      case _ => t
    }

    // Append a result variable and its type.
    val resultVar = Symbol.freshVariableSymbol("r")
    val result = (resultVar, t.typ.resultType)

    val args = (findArgs(t) ::: result :: Nil).map {
      case (x, typ) => Lst(List(Lit(x.s), Lit(types.getName(typ))))
    }
    val k: (SmtExp) => Lst = e => Lst(List(Lit("="), Lit(resultVar.s), e))
    val body = compileTerm(findBody(t), k)

    val r = Lst(List(Lit("define-fun"), SmtExp.Lst(args), Lit("Bool"), body))
    println(r.fmt(0))

    r
  }

  val id = identity[SmtExp] _
  
  /**
   * Returns the given term `t` as an SMT-LIB expression.
   */
  def compileTerm(t: Term, k: SmtExp => SmtExp): SmtExp = t match {
    case Term.Unit => k(Lit("mk-unit"))
    case Term.Bool(b) => k(Lit(b.toString))
    case Term.Int(i) => k(Lit(i.toString))

    case Term.Var(x) => k(Lit(x.s))

    case Term.Match(t1, rules) => Lst(Lit("or") :: compileRules(t1, rules, k))

    case Term.Tag(x, t1, _) => Lst(List(Lit("mk-" + x.s), compileTerm(t1, id)))
    case Term.Tuple2(t1, t2) => Lst(List(Lit("mk2"), compileTerm(t1, id), compileTerm(t2, id)))
    case Term.Tuple3(t1, t2, t3) => Lst(List(Lit("mk3"), compileTerm(t1, id), compileTerm(t2, id), compileTerm(t3, id)))
    case Term.Tuple4(t1, t2, t3, t4) => Lst(List(Lit("mk4"), compileTerm(t1, id), compileTerm(t2, id), compileTerm(t3, id), compileTerm(t4, id)))
    case Term.Tuple5(t1, t2, t3, t4, t5) => Lst(List(Lit("mk5"), compileTerm(t1, id), compileTerm(t2, id), compileTerm(t3, id), compileTerm(t4, id), compileTerm(t5, id)))
  }

  /**
   * Returns the given rules `rs` as an SMT-LIB expression.
   */
  def compileRules(mt: Term, rs: List[(Pattern, Term)], k: SmtExp => SmtExp): List[SmtExp] =
    rs.map {
      case (p, t) => compileRule(mt, p, t, k)
    }

  /**
   * Return an SMT-LIB expression for the given match value `mt` on pattern `p` and rule body `t`.
   */
  def compileRule(mt: Term, p: Pattern, t: Term, k: SmtExp => SmtExp): SmtExp =
      Lst(List(Lit("and"), Lst(List(Lit("="), compileTerm(mt, id), compilePattern(p))), compileTerm(t, k)))

  def compilePattern(p: Pattern): SmtExp = p match {
    case Pattern.Wildcard => Lit(Symbol.freshVariableSymbol("free_").s)
    case Pattern.Var(x) => Lit(x.s)
      
    case Pattern.Unit => Lit("unit")

    case Pattern.Tag(n, p1) => Lst(List(Lit(n.s), compilePattern(p1)))

    case Pattern.Tuple2(p1, p2) => Lst(List(Lit("mk2"), compilePattern(p1), compilePattern(p2)))
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

  //  def foo(typ: Type): List[SmtExp] = {
  //    val types = mutable.Map.empty[Type, String]
  //    val result = mutable.ListBuffer.empty[SmtExp]
  //    def visit(typ: Type): String = typ match {
  //      case Type.Unit => "unit"
  //      case Type.Bool => "bool"
  //      case Type.Int => "int"
  //      case Type.Str => "str"
  //
  //      case Type.Tuple2(typ1, typ2) =>
  //        val (sortName, fieldSort1, fieldSort2) = (freshTypeName, visit(typ1), visit(typ2))
  //        types += typ -> sortName
  //        result += SmtExp.Lst(List(SmtExp.Literal("declare-type"),  SmtExp.Lst(Nil), SmtExp.Literal(sortName),
  //          SmtExp.Lst(List(SmtExp.Literal(freshTypeName), SmtExp.Literal(fieldSort1))),
  //          SmtExp.Lst(List(SmtExp.Literal(freshTypeName), SmtExp.Literal(fieldSort2)))))
  //        sortName
  //      case Type.Tuple3(typ1, typ2, typ3) =>
  //        val (sortName, fieldSort1, fieldSort2, fieldSort3) =  (freshTypeName, visit(typ1), visit(typ2), visit(typ3))
  //        types += typ -> sortName
  //        result += SmtExp.Lst(List(SmtExp.Literal("declare-type"), SmtExp.Lst(Nil), SmtExp.Literal(sortName), SmtExp.Literal(fieldSort1), SmtExp.Literal(fieldSort2), SmtExp.Literal(fieldSort3)))
  //        sortName
  //
  //    }
  //    visit(typ)
  //
  //    result.toList
  //  }


}
