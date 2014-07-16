package impl.runtime

import java.io.{File, PrintWriter}

import impl.logic.Symbol.{LatticeSymbol => LSym, PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import impl.verifier._
import syntax.Symbols._
import syntax._

/**
 * A verifier / type checker.
 */
class Verifier(val program: Program) {

  /**
   * Verifies that the program is safe.
   */
  def verify(): Unit = {

    for ((s, lattice) <- program.lattices) {
      val file = new File("./z3/" + lattice.name.s + "0.smt")
      val writer = new PrintWriter(file)

      writer.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      writer.println(";;;; AUTOMATICALLY GENERATED FILE. DO NOT EDIT.                              ;;")
      writer.println(";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;")
      writer.println()

      writer.println(datatype(lattice.name, lattice.domain).fmt)
      writer.println(relation2(lattice.name, lattice.leq).fmt)
      writer.println(relation3(lattice.name, lattice.lub).fmt)

      writer.println(LatticeLeq.reflexivity(lattice.name, lattice.leq))
      writer.println(LatticeLeq.antiSymmetri(lattice.name, lattice.leq))
      writer.println(LatticeLeq.transitivity(lattice.name, lattice.leq))
      writer.println(LatticeLeq.leastElement(lattice.name, lattice.bot, lattice.leq))

      writer.println(Function2.isFunction(lattice.name, lattice.lub))
      writer.println(Function2.isTotal(lattice.name, lattice.lub))
      writer.println(LatticeLub.upperBound(lattice.name, lattice.leq, lattice.lub))
      writer.println(LatticeLub.leastUpperBound(lattice.name, lattice.leq, lattice.lub))

      for (s <- List(Symbol.PredicateSymbol("Sign.Sum"))) {
        // TODO: Need to find transfer functions...
        writer.println(Function2.isFunction(lattice.name, s))
        writer.println(Function2.isTotal(lattice.name, s))
        writer.println(Transfer.isStrict2(lattice.name, lattice.bot, s))
        writer.println(Transfer.isMonotone2(lattice.name, s, lattice.leq))
      }

      // TODO: Need termination function.
      val h = Symbol.PredicateSymbol("Sign.Height")
      writer.println(Function1.isFunction(lattice.name, h))
      writer.println(Function1.isTotal(lattice.name, h))
      writer.println(Termination.strictlyDecreasing(lattice.name, h, lattice.leq))
      writer.println(Termination.nonNegative(lattice.name, h))


      writer.close();
    }

    println()
    println()
  }

  /**
   * Returns a datatype declaration for the given lattice symbol `l` and type `t`.
   */
  def datatype(l: LSym, t: Type): Declaration = t match {
    case Type.Variant(ts) => Declaration.Datatype(l, ts.toList.map(_.asInstanceOf[Type.Constructor0].name))
  }

  /**
   * Returns an SMT formula for function defined by the predicate symbol `s` with the given `sort`.
   */
  def relation2(sort: LSym, s: PSym): Declaration = {
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

    Declaration.Relation2(s, sort, x, y, formulae)
  }

  /**
   * Returns an SMT formula for function defined by the predicate symbol `s` with the given `sort`.
   */
  def relation3(sort: LSym, s: PSym): Declaration = {
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

    Declaration.Relation3(s, sort, x, y, z, formulae)
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

  /**
   * An SMT-LIB declaration.
   */
  sealed trait Declaration {
    def fmt: String = this match {
      case Declaration.Relation2(s, sort, var1, var2, formula) =>
        smt"(define-fun $s (($var1 $sort) ($var2 $sort)) Bool" + "\n    " + formula.fmt(1) + ")\n"

      case Declaration.Relation3(s, sort, var1, var2, var3, formula) =>
        smt"(define-fun $s (($var1 $sort) ($var2 $sort) ($var3 $sort)) Bool" + "\n    " + formula.fmt(1) + ")\n"

      case Declaration.Datatype(s, variants) =>
        smt"(declare-datatypes () (($s " + variants.map(_.s).mkString(" ") + ")))\n"
    }
  }

  object Declaration {

    /**
     * A 2-ary boolean function declaration.
     */
    case class Relation2(name: Symbol.PredicateSymbol, sort: Symbol.LatticeSymbol, var1: VSym, var2: VSym, formula: SmtFormula) extends Declaration

    /**
     * A 3-ary boolean function declaration.
     */
    case class Relation3(name: Symbol.PredicateSymbol, sort: Symbol.LatticeSymbol, var1: VSym, var2: VSym, var3: VSym, formula: SmtFormula) extends Declaration

    /**
     * A datatype declaration.
     */
    case class Datatype(name: Symbol.LatticeSymbol, variants: List[Symbol.NamedSymbol]) extends Declaration

  }

  /**
   * An SMT-LIB formula.
   */
  sealed trait SmtFormula {
    def variables: Set[Symbol.VariableSymbol] = this match {
      case SmtFormula.True => Set.empty
      case SmtFormula.False => Set.empty
      case SmtFormula.Variable(s) => Set(s)
      case SmtFormula.Constructor0(s) => Set.empty
      case SmtFormula.Conjunction(formulae) => (Set.empty[VSym] /: formulae)((xs, f) => xs ++ f.variables)
      case SmtFormula.Disjunction(formulae) => (Set.empty[VSym] /: formulae)((xs, f) => xs ++ f.variables)
      case SmtFormula.Eq(lhs, rhs) => lhs.variables ++ rhs.variables
    }

    def fmt(indent: Int): String = this match {
      case SmtFormula.True => "true"
      case SmtFormula.False => "false"
      case SmtFormula.Variable(s) => s.fmt
      case SmtFormula.Constructor0(s) => s.fmt
      case SmtFormula.Conjunction(formulae) => "(and " + formulae.map(_.fmt(indent)).mkString(" ") + ")"
      case SmtFormula.Disjunction(formulae) => "(or \n" + "    " * (indent + 1) + formulae.map(_.fmt(indent + 1)).mkString("\n" + "    " * (indent + 1)) + ")"
      case SmtFormula.Eq(lhs, rhs) => "(= " + lhs.fmt(indent) + " " + rhs.fmt(indent) + ")"
    }
  }

  object SmtFormula {

    /**
     * The true literal.
     */
    case object True extends SmtFormula

    /**
     * The false literal.
     */
    case object False extends SmtFormula

    /**
     * A variable.
     */
    case class Variable(v: Symbol.VariableSymbol) extends SmtFormula

    /**
     * A equality formula.
     */
    case class Eq(left: SmtFormula, right: SmtFormula) extends SmtFormula

    /**
     * A conjunction of formulae.
     */
    case class Conjunction(formulae: List[SmtFormula]) extends SmtFormula

    /**
     * A disjunction of formulae.
     */
    case class Disjunction(formulae: List[SmtFormula]) extends SmtFormula

    /**
     * An implication of antecedent => consequent.
     */
    case class Implication(antecedent: SmtFormula, consequent: SmtFormula) extends SmtFormula

    /**
     * A null-ary constructor.
     */
    case class Constructor0(s: Symbol.NamedSymbol) extends SmtFormula

    /**
     * A 1-ary constructor.
     */
    case class Constructor1(s: Symbol.NamedSymbol, f1: SmtFormula) extends SmtFormula

    /**
     * A 2-ary constructor.
     */
    case class Constructor2(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula) extends SmtFormula

    /**
     * A 3-ary constructor.
     */
    case class Constructor3(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula, f3: SmtFormula) extends SmtFormula

    /**
     * A 4-ary constructor.
     */
    case class Constructor4(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula, f3: SmtFormula, f4: SmtFormula) extends SmtFormula

    /**
     * A 5-ary constructor.
     */
    case class Constructor5(s: Symbol.NamedSymbol, f1: SmtFormula, f2: SmtFormula, f3: SmtFormula, f4: SmtFormula, f5: SmtFormula) extends SmtFormula

  }

}
