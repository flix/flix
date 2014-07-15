package impl.runtime

import impl.logic.Symbol.{PredicateSymbol => PSym, VariableSymbol => VSym}
import impl.logic._
import syntax.Symbols._

/**
 * A verifier / type checker.
 */
class Verifier(val program: Program) {

  /**
   * Verifies that the program is safe.
   */
  def verify(): Unit = {

    println("Proof Burdens")
    // TODO: Need better access to lattices
    for ((p, i) <- program.interpretation) {
      i match {
        case Interpretation.LatticeMap(lattice) => {
          println(reflexivity(p, lattice.leq))
          println(antiSymmetri(p, lattice.leq))

          println("~~~~~~~~")
          println(genLeq(lattice.leq).fmt)
          println("~~~~~~~~")

        }
        case _ => // nop
      }
    }

    println()
    println()
  }

  /**
   * Reflexivity: ∀x. x ⊑ x
   */
  def reflexivity(sort: PSym, leq: PSym): String = smt"""
    |;; Reflexivity: ∀x. x ⊑ x
    |(define-fun reflexivity () Bool
    |    (forall ((x $sort))
    |        ($leq x x)))
    """.stripMargin

  /**
   * Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
   */
  def antiSymmetri(sort: PSym, leq: PSym): String = smt"""
    |;; Anti-symmetri: ∀x, y. x ⊑ y ∧ x ⊒ y ⇒ x = y
    |(define-fun anti-symmetri () Bool
    |    (forall ((x $sort) (y $sort))
    |        (=>
    |            (and ($leq x y)
    |                 ($leq y x))
    |            (= x y))))
    """.stripMargin


  def genDatatype: String = ???

  def genLeq(s: PSym): Declaration = {
    //    val Leq = List(
    //      HornClause(Predicate(LeqSymbol, List(Bot, Term.Variable("_")))),
    //      HornClause(Predicate(LeqSymbol, List(Neg, Neg))),
    //      HornClause(Predicate(LeqSymbol, List(Zero, Zero))),
    //      HornClause(Predicate(LeqSymbol, List(Pos, Pos))),
    //      HornClause(Predicate(LeqSymbol, List(Term.Variable("_"), Top)))
    //    )
    //    (define-fun Sign.leq ((x Sign) (y Sign)) Bool
    //      (or (= x Sign.Bot)
    //    (and (= x Sign.Neg) (= y Sign.Neg))
    //    (and (= x Sign.Zer) (= y Sign.Zer))
    //    (and (= x Sign.Pos) (= y Sign.Pos))
    //    (= y Sign.Top)))

    val clauses = program.clauses.filter(_.head.name == s)

    val p = Predicate(s, List(Term.Variable("x"), Term.Variable("y")))

    val formulae = SmtFormula.Disjunction(clauses.map {
      h => Unification.unify(h.head, p, Map.empty[VSym, Term]) match {
        case None => SmtFormula.True // nop
        case Some(env) =>
          if (h.isFact)
            genEnv(env)
          else
            SmtFormula.True // TODO
      }
    })

    Declaration.Function2(s, "x", "y", formulae)
  }

  def genJoin: SmtFormula = ???

  def genTransfer2: SmtFormula = ???

  def genEnv(env: Map[VSym, Term]): SmtFormula = SmtFormula.Conjunction(env.toList.map {
    case (v, t) => t match {
      case Term.Bool(true) => SmtFormula.True
      case Term.Variable(_) => SmtFormula.True
      case Term.Constructor0(s) => SmtFormula.Eq(SmtFormula.Variable(v), SmtFormula.Constructor0(s))
    }
  })


  sealed trait Declaration {
    def fmt: String = this match {
      case Declaration.Function2(s, var1, var2, formula) =>
        smt"""(define-fun $s (($var1 Sign) ($var2 Sign)) Bool
              |    ${formula.fmt(2)})
           """.stripMargin
    }
  }

  object Declaration {

    // TODO: Need sort
    case class Function2(name: Symbol.PredicateSymbol, var1: String, var2: String, formula: SmtFormula) extends Declaration

  }

  sealed trait SmtFormula {
    def fmt(indent: Int): String = this match {
      case SmtFormula.True => "true"
      case SmtFormula.False => "false"
      case SmtFormula.Variable(s) => s.fmt
      case SmtFormula.Constructor0(s) => s.fmt
      case SmtFormula.Conjunction(formulae) =>
        "(and " + formulae.map(_.fmt(indent)).mkString(" ") + ")"
      case SmtFormula.Disjunction(formulae) =>
        "(or \n" + "    " * indent + formulae.map(_.fmt(indent + 1)).mkString("\n" + "     " * indent) + ")"
      case SmtFormula.Eq(lhs, rhs) => "(= " + lhs.fmt(indent) + " " + rhs.fmt(indent) + ")"

    }
  }

  object SmtFormula {

    case object True extends SmtFormula

    case object False extends SmtFormula

    case class Variable(v: Symbol.VariableSymbol) extends SmtFormula

    case class Constructor0(s: Symbol.NamedSymbol) extends SmtFormula

    case class Conjunction(formulae: List[SmtFormula]) extends SmtFormula

    case class Disjunction(formulae: List[SmtFormula]) extends SmtFormula

    case class Implication(left: SmtFormula, right: SmtFormula) extends SmtFormula

    case class ForAll() extends SmtFormula

    case class Exists() extends SmtFormula

    case class Eq(left: SmtFormula, right: SmtFormula) extends SmtFormula


  }

  /**
   * A string interpolator which takes symbols into account.
   */
  implicit class SmtSyntaxInterpolator(sc: StringContext) {
    def smt(args: Any*): String = {
      def format(a: Any): String = a match {
        case x: Symbol => x.fmt
        case x => x.toString
      }

      val pi = sc.parts.iterator
      val ai = args.iterator
      val bldr = new java.lang.StringBuilder(pi.next())
      while (ai.hasNext) {
        bldr append format(ai.next())
        bldr append pi.next()
      }
      bldr.toString
    }
  }

}
