package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.TypedAst.Expression.Var
import ca.uwaterloo.flix.language.ast.{SourcePosition, Name, SourceLocation, TypedAst}

object Verifier {

  sealed trait Property {
    val exp: TypedAst.Expression
  }

  object Theorems {

    /**
      * Partial Order Theorems.
      */
    object PartialOrder {

      // TODO: Use UTF8 symbols?

      /**
        * Reflexivity: \forall x. x <= x.
        */
      case class Reflexivity(lattice: TypedAst.Definition.BoundedLattice) {
        val exp = {
          val (tpe, leq) = (lattice.tpe, lattice.leq)

          leq(mkVar("x", tpe), mkVar("x", tpe))
        }
      }

      def antiSymmetry(lattice: TypedAst.Definition.BoundedLattice): TypedAst.Expression =
        ???

      /**
        * Transitivity: \forall x, y, z. x <= y /\ y <= z ==> x <= z
        */
      case class Transitivity(lattice: TypedAst.Definition.BoundedLattice) {
        val exp = {
          // extract lattice components.
          val TypedAst.Definition.BoundedLattice(tpe, bot, top, leq, lub, glb, loc) = lattice

          val ⊑ = leq
          // val TypedAst.Definition.BoundedLattice(tpe, ⊥, ⊤, ⊑, ⊔, ⊓, loc) = lattice

          // declare the variables and their types.
          val (x, y, z) = (mkVar("x", tpe), mkVar("y", tpe), mkVar("z", tpe))


          (x ⊑ y) ∧ (y ⊑ z) → (x ⊑ z)
        }
      }

    }

  }

  def theorems: Map[TypedAst.Definition.BoundedLattice, Set[Property]] = ???

  /**
    * Returns a typed AST node that represents a variable of the given `name` and with the given type `tpe`.
    */
  private def mkVar(name: String, tpe: TypedAst.Type): TypedAst.Expression.Var =
    TypedAst.Expression.Var(Name.Ident(SourcePosition.Unknown, "x", SourcePosition.Unknown), tpe, SourceLocation.Unknown)

  implicit class RichExp(val e: TypedAst.Expression) {
    def apply(e1: TypedAst.Expression): TypedAst.Expression = ???

    def apply(e1: TypedAst.Expression, e2: TypedAst.Expression): TypedAst.Expression =
      TypedAst.Expression.Apply(e, List(e1, e2), ???, SourceLocation.Unknown) // TODO: What is the tpe?

    def ∧(that: TypedAst.Expression): TypedAst.Expression = ???

    def →(that: TypedAst.Expression): TypedAst.Expression = ???

    def ⊑(that: TypedAst.Expression): TypedAst.Expression = ???

    def ⊔(that: TypedAst.Expression): TypedAst.Expression = ???
  }

  //  /**
  //   * Anti-symmetri: ?x, y. x ? y ? x ? y ? x = y
  //   */
  //  def antiSymmetri(leq: Term.Abs): Term.Abs =
  //    Term.Abs('x, leq.typ, Term.Abs('y, leq.typ,
  //      (leq.call('x, 'y) && leq.call('y, 'x)) ==> (Term.Var('x) === Term.Var('y))))
  //
  //  /**
  //   * Transitivity: ?x, y, z. x ? y ? y ? z ? x ? z.
  //   */
  //  def transitivity(leq: Term.Abs): Term.Abs =
  //    Term.Abs('x, leq.typ, Term.Abs('y, leq.typ, Term.Abs('z, leq.typ,
  //      (leq.call('x, 'y) && leq.call('y, 'z)) ==> leq.call('x, 'z))))
  //
  //  /**
  //   * Least Element: ?x. ? ? x.
  //   */
  //  def leastElement(leq: Term.Abs, bot: Value): Term.Abs =
  //    Term.Abs('x, leq.typ,
  //      leq.call(bot.toTerm, 'x))
  //
  //
  //  /**
  //   * Upper Bound: ?x, y. x ? (x ? y) ? y ? (x ? y).
  //   */
  //  def upperBound(leq: Term.Abs, lub: Term.Abs): Term.Abs =
  //    Term.Abs('x, leq.typ, Term.Abs('y, leq.typ,
  //      leq.call('x, lub.call('x, 'y)) && leq.call('y, lub.call('x, 'y))))
  //
  //  /**
  //   * Least Upper Bound: ?x, y, z. x ? z ? y ? z ? x ? y ? z.
  //   */
  //  def leastUpperBound(leq: Term.Abs, lub: Term.Abs): Term.Abs =
  //    Term.Abs('x, leq.typ, Term.Abs('y, leq.typ, Term.Abs('z, leq.typ,
  //      (leq.call('x, 'z) && leq.call('y, 'z)) ==> leq.call(lub.call('x, 'y), 'z))))
  //
  //
  //  /**
  //   * Non-Negative: ?x. f(x) > 0.
  //   */
  //  def nonNegative(h: Term.Abs): Term.Abs =
  //    Term.Abs('x, h.typ,
  //      Term.BinaryOp(BinaryOperator.Greater, h.call('x), Term.Int(0)))
  //
  //  /**
  //   * Stricly-Decreasing: ?x, y. x ? y ? x != y ? f(x) > f(y).
  //   */
  //  def strictlyDecreasing(h: Term.Abs, leq: Term.Abs): Term.Abs =
  //    Term.Abs('x, h.typ, Term.Abs('y, h.typ,
  //      (leq.call('x, 'y) && (Term.Var('x) !== Term.Var('y))) ==>
  //        Term.BinaryOp(BinaryOperator.Greater, h.call('x), h.call('y))))
  //
  //  /**
  //   * Monotone: ?x1, x2. x1 ? x2 ? f(x1) ? f(x2).
  //   */
  //  def monotone1(f: Term.Abs, leq: Term.Abs): Term.Abs =
  //    Term.Abs('x1, leq.typ, Term.Abs('x2, leq.typ,
  //      leq.call('x1, 'x2) ==> leq.call(f.call('x1), f.call('x2))))
  //
  //  /**
  //   * Monotone: ?x1, x2, y1, y2. x1 ? x2 ? y1 ? y2 ? f(x1, y1) ? f(x2, y2).
  //   */
  //  def monotone2(f: Term.Abs, leq: Term.Abs): Term.Abs =
  //    Term.Abs('x1, leq.typ, Term.Abs('x2, leq.typ, Term.Abs('y1, leq.typ, Term.Abs('y2, leq.typ,
  //      (leq.call('x1, 'x2) && leq.call('y1, 'y2)) ==> leq.call(f.call('x1, 'y1), f.call('x2, 'y2))))))

}
