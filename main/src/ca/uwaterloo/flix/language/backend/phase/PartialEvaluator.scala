package ca.uwaterloo.flix.language.backend.phase

import ca.uwaterloo.flix.language.ast.{UnaryOperator, BinaryOperator, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Expression._
import ca.uwaterloo.flix.language.ast.TypedAst.Literal

object PartialEvaluator {

  def eval(e: TypedAst.Expression): TypedAst.Expression =
    eval(e, x => x)

  def eval(e: TypedAst.Expression, k: TypedAst.Expression => TypedAst.Expression): TypedAst.Expression = e match {
    case Lit(literal, tpe, loc) => k(e)

    case Unary(UnaryOperator.UnaryMinus, exp, _, _) => eval(exp, e => e match {
      case Lit(Literal.Int(i, loc2), tpe, loc1) => k(Lit(Literal.Int(-i, loc2), tpe, loc1))
    })

    case Binary(BinaryOperator.Or, exp1, exp2, _, _) => eval(exp1, e1 => e1 match {
      case Lit(Literal.Bool(true, loc), tpe, _) => k(Lit(Literal.Bool(lit = true, loc), tpe, loc))
      case _ => eval(exp2, e2 => e2 match {
        case Lit(Literal.Bool(true, loc), tpe, _) => k(Lit(Literal.Bool(lit = true, loc), tpe, loc))
        case _ => ??? // TODO: Reconstruct term.
      })
    })

  }

}
