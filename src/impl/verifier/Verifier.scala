package impl.verifier

import java.io.{File, PrintWriter}

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import impl.runtime.Unification
import syntax.Symbols._

/**
 * A verifier / type checker.
 */
class Verifier(val program: Program) {

  val Z3 = System.getProperty("Z3", "C:\\Program Files\\Microsoft Z3\\z3-4.3.0-x64\\bin\\z3.exe")

  /**
   * Verifies that the program is safe.
   */
  def verify(): Unit = {
    for ((_, lattice) <- program.lattices) {
      emitVerificationConditions(lattice)
    }
  }

  /**
   * Emit verifications conditions.
   */
  private def emitVerificationConditions(lattice: Lattice): Unit = {
    /**
     * Verification conditions for the lattice ordering: Leq.
     */
    run(lattice, "Leq is reflexivity", LatticeLeq.reflexivity(lattice.name, lattice.leq))
    run(lattice, "Leq is antisymmetric", LatticeLeq.antiSymmetri(lattice.name, lattice.leq))
    run(lattice, "Leq is transitive", LatticeLeq.transitivity(lattice.name, lattice.leq))
    run(lattice, "Leq has a bottom element", LatticeLeq.leastElement(lattice.name, lattice.bot, lattice.leq))

    /**
     * Verification conditions for the lattice least-upper-bound: Lub.
     */
    run(lattice, "Lub is a function", Function2.isFunction(lattice.name, lattice.lub))
    run(lattice, "Lub is total", Function2.isTotal(lattice.name, lattice.lub))
    run(lattice, "Lub is an upper bound", LatticeLub.upperBound(lattice.name, lattice.leq, lattice.lub))
    run(lattice, "Lub is a least upper bound", LatticeLub.leastUpperBound(lattice.name, lattice.leq, lattice.lub))

    /**
     * Verification conditions for the lattice height.
     */
    run(lattice, "The height function is a function", Function1.isFunction(lattice.name.fmt, "Int", lattice.height))
    run(lattice, "The height function is total", Function1.isTotal(lattice.name.fmt, "Int", lattice.height))
    run(lattice, "The height function is strictly decreasing", LatticeHeight.strictlyDecreasing(lattice.name, lattice.height, lattice.leq))
    run(lattice, "The height function is always non-negative", LatticeHeight.nonNegative(lattice.name, lattice.height))

    for (s <- lattice.funcs) {
      run(lattice, s.fmt + " is a function", Function2.isFunction(lattice.name, s))
      run(lattice, s.fmt + " is a total", Function2.isTotal(lattice.name, s))
      run(lattice, s.fmt + " is a strict", Transfer.isStrict2(lattice.name, lattice.bot, s))
      run(lattice, s.fmt + " is monotone", Transfer.isMonotone2(lattice.name, s, lattice.leq))
    }
  }

  /**
   * Run Z3 on the given input.
   */
  def run(lattice: Lattice, name: String, s: String): Unit = {
    // Create a temporary file and store the lattice declarations and constraints.
    val tmpFile = File.createTempFile("z3-constraint", ".txt");
    val writer = new PrintWriter(tmpFile)
    writer.println(declarations(lattice))
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
   * Returns the lattice declarations.
   */
  private def declarations(lattice: Lattice): String = {
    val sb = new StringBuilder()
    sb.append(datatype(lattice.name, lattice.domain).fmt)
    sb.append("\n")
    sb.append(relation2(lattice.leq, List(lattice.name.fmt, lattice.name.fmt)).fmt)
    sb.append("\n")
    sb.append(relation3(lattice.lub, List(lattice.name.fmt, lattice.name.fmt, lattice.name.fmt)).fmt)
    sb.append("\n")
    sb.append(relation2(lattice.height, List(lattice.name.fmt, "Int")).fmt)
    sb.append("\n")
    for (s <- lattice.funcs) {
      // TODO: Assumes fixed arity...
      sb.append(relation3(s, List(lattice.name.fmt, lattice.name.fmt, lattice.name.fmt)).fmt)
      sb.append("\n")
    }
    sb.toString()
  }

  /**
   * Returns a datatype declaration for the given lattice symbol `l` and type `t`.
   */
  def datatype(l: LSym, t: Type): SmtDeclaration = ???

  /**
   * Returns an SMT formula for function defined by the predicate symbol `s` with the given `sort`.
   */
  def relation2(s: PSym, sorts: List[String]): SmtDeclaration = {
    val clauses = program.clauses.filter(_.head.name == s)

    val (x, y) = (Symbol.freshVariableSymbol("x"), Symbol.freshVariableSymbol("y"))

    val p = Predicate(s, List(Term.Var(x), Term.Var(y)), ???)
    val formulae = SmtFormula.Disjunction(clauses.map {
      h => Unification.unify(h.head, p, Map.empty[VSym, Term]) match {
        case None => SmtFormula.True // nop
        case Some(env) =>
          if (h.isFact)
            asFormula(Set(x, y), env)
          else
            SmtFormula.True // TODO
      }
    })

    SmtDeclaration.Relation(s, sorts, List(x, y), formulae)
  }

  /**
   * Returns an SMT formula for function defined by the predicate symbol `s` with the given `sort`.
   */
  def relation3(s: PSym, sorts: List[String]): SmtDeclaration = {
    val clauses = program.clauses.filter(_.head.name == s)

    // TODO: Need to genereate fresh symbols.
    val (x, y, z) = (Symbol.VariableSymbol("x0"), Symbol.VariableSymbol("y0"), Symbol.VariableSymbol("z0"))

    val p = Predicate(s, List(Term.Var(x), Term.Var(y), Term.Var(z)), ???)
    val formulae = SmtFormula.Disjunction(clauses.map {
      h => Unification.unify(p, h.head, Map.empty[VSym, Term]) match {
        case None => SmtFormula.True // nop
        case Some(env) =>
          if (h.isFact)
            asFormula(Set(x, y, z), env)
          else
            SmtFormula.True // TODO
      }
    })

    SmtDeclaration.Relation(s, sorts, List(x, y, z), formulae)
  }

  /**
   * TODO: DOC
   */
  def asFormula(bound: Set[VSym], env: Map[VSym, Term]): SmtFormula = SmtFormula.Conjunction(env.toList.flatMap {
    case (v, t) => {
      val f = SmtFormula.Eq(SmtFormula.Variable(v), asFormula(t, env))
      if (f.variables.exists(s => !(bound contains s))) {
        // A free variable exists in the formula. Ignore the clause.
        None
      } else
        Some(f)
    }
  })

  /**
   * Returns the given term `t` as a SMT-LIB formula under the given environment `env`.
   */
  def asFormula(t: Term, env: Map[VSym, Term]): SmtFormula = t match {
    case Term.Bool(b) => if (b) SmtFormula.True else SmtFormula.False
    case Term.Int(i) => SmtFormula.Int(i)
    case Term.Var(s) => env.get(s) match {
      case None => SmtFormula.Variable(s)
      case Some(tt) => asFormula(tt, env)
    }
  }


}
