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
          println(genLeq(lattice.leq))
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

    val formulae = Formula.Disjunction(clauses.map {
      h => Unification.unify(h.head, p, Map.empty[VSym, Term]) match {
        case None => Formula.True // nop
        case Some(env) =>
          if (h.isFact)
            genEnv(env)
          else
            Formula.True // TODO
      }
    })

    Declaration.Relation3("x", "y", formulae)
  }

  def genJoin: Formula = ???

  def genTransfer2: Formula = ???

  def genEnv(env: Map[VSym, Term]): Formula = Formula.Conjunction(env.toList.map {
    case (v, t) => t match {
      case Term.Bool(true) => Formula.True
      case Term.Constructor0(s) => Formula.Eq(v, s)
    }
  })


  sealed trait Declaration

  object Declaration {

    case class Relation3(var1: String, var2: String, formula: Formula) extends Declaration

  }

  sealed trait Formula

  object Formula {

    case class Conjunction(formulae: List[Formula]) extends Formula

    case class Disjunction(formulae: List[Formula]) extends Formula

    case class Implication(left: Formula, right: Formula) extends Formula

    case class ForAll() extends Formula

    case class Exists() extends Formula

    case class Eq(left: Any, right: Any) extends Formula

    case object True extends Formula

    case object False extends Formula


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
