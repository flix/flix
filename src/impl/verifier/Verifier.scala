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
    val file = new File("./z3/" + lattice.name.s + "0.smt")
    val writer = new PrintWriter(file)

    writer.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    writer.println(";;;; AUTOMATICALLY GENERATED FILE. DO NOT EDIT.                              ;;")
    writer.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
    writer.println()

    writer.println(datatype(lattice.name, lattice.domain).fmt)
    writer.println(relation2(lattice.name, lattice.leq).fmt)
    writer.println(relation3(lattice.name, lattice.lub).fmt)

    latticeLeq(lattice, writer)
    latticeLub(lattice, writer)
    latticeHeight(lattice, writer)

    for (s <- List(Symbol.PredicateSymbol("Sign.Sum"))) {
      // TODO: Need to find transfer functions...
      writer.println(Function2.isFunction(lattice.name, s))
      writer.println(Function2.isTotal(lattice.name, s))
      writer.println(Transfer.isStrict2(lattice.name, lattice.bot, s))
      writer.println(Transfer.isMonotone2(lattice.name, s, lattice.leq))
    }

    writer.close();
  }

  /**
   * Verifications conditions for lattice order.
   */
  def latticeLeq(lattice: Lattice, writer: PrintWriter): Unit = {
    writer.println(LatticeLeq.reflexivity(lattice.name, lattice.leq))
    writer.println(LatticeLeq.antiSymmetri(lattice.name, lattice.leq))
    writer.println(LatticeLeq.transitivity(lattice.name, lattice.leq))
    writer.println(LatticeLeq.leastElement(lattice.name, lattice.bot, lattice.leq))
  }

  /**
   * Verifications conditions for lattice least-upper-bound
   */
  def latticeLub(lattice: Lattice, writer: PrintWriter): Unit = {
    writer.println(Function2.isFunction(lattice.name, lattice.lub))
    writer.println(Function2.isTotal(lattice.name, lattice.lub))
    writer.println(LatticeLub.upperBound(lattice.name, lattice.leq, lattice.lub))
    writer.println(LatticeLub.leastUpperBound(lattice.name, lattice.leq, lattice.lub))
  }

  /**
   * Verifications conditions for lattice height.
   */
  def latticeHeight(lattice: Lattice, writer: PrintWriter): Unit = {
    writer.println(Function1.isFunction(lattice.name.fmt, "Int", lattice.height))
    writer.println(Function1.isTotal(lattice.name.fmt, "Int", lattice.height))
    writer.println(LatticeHeight.strictlyDecreasing(lattice.name, lattice.height, lattice.leq))
    writer.println(LatticeHeight.nonNegative(lattice.name, lattice.height))
  }

  /**
   * Returns a datatype declaration for the given lattice symbol `l` and type `t`.
   */
  def datatype(l: LSym, t: Type): SmtDeclaration = t match {
    case Type.Variant(ts) => SmtDeclaration.Datatype(l, ts.toList.map(_.asInstanceOf[Type.Constructor0].name))
  }

  /**
   * Returns an SMT formula for function defined by the predicate symbol `s` with the given `sort`.
   */
  def relation2(sort: LSym, s: PSym): SmtDeclaration = {
    val clauses = program.clauses.filter(_.head.name == s)

    val (x, y) = (Symbol.freshVariableSymbol("x"), Symbol.freshVariableSymbol("y"))

    val p = Predicate(s, List(Term.Variable(x), Term.Variable(y)))
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

    SmtDeclaration.Relation2(s, sort, x, y, formulae)
  }

  /**
   * Returns an SMT formula for function defined by the predicate symbol `s` with the given `sort`.
   */
  def relation3(sort: LSym, s: PSym): SmtDeclaration = {
    val clauses = program.clauses.filter(_.head.name == s)

    // TODO: Need to genereate fresh symbols.
    val (x, y, z) = (Symbol.VariableSymbol("x0"), Symbol.VariableSymbol("y0"), Symbol.VariableSymbol("z0"))

    val p = Predicate(s, List(Term.Variable(x), Term.Variable(y), Term.Variable(z)))
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

    SmtDeclaration.Relation3(s, sort, x, y, z, formulae)
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
    case Term.Variable(s) => env.get(s) match {
      case None => SmtFormula.Variable(s)
      case Some(tt) => asFormula(tt, env)
    }
    case Term.Constructor0(s) => SmtFormula.Constructor0(s)
  }

}
