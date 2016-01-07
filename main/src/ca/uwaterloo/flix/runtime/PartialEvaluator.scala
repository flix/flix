package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.TypedAst.Expression._
import ca.uwaterloo.flix.language.ast.TypedAst.Literal
import ca.uwaterloo.flix.language.ast.{BinaryOperator, TypedAst, UnaryOperator}
import ca.uwaterloo.flix.language.backend.ir.ReducedIR

object PartialEvaluator {

  type Cont = TypedAst.Expression => TypedAst.Expression

  // TODO: Introduce type for k.
  // TODO: pass env
  def eval(exp0: TypedAst.Expression, env0: Map[Int, ReducedIR.Expression], k: Cont): TypedAst.Expression = exp0 match {
    case Lit(literal, tpe, loc) => k(exp0)

    case Unary(UnaryOperator.Minus, exp, _, _) => eval(exp, env0, {
      case Lit(Literal.Int(i, loc2), tpe, loc1) => k(Lit(Literal.Int(-i, loc2), tpe, loc1))
    })

    case exp@Binary(BinaryOperator.Or, exp1, exp2, _, _) =>
      // partially evaluate exp1
      eval(exp1, env0, e1 => e1 match {
        // Case 1: exp1 is true. The result is true.
        case Lit(Literal.Bool(true, loc), tpe, _) => k(Lit(Literal.Bool(lit = true, loc), tpe, loc))
        // Case 2: exp1 is false. The result is exp2.
        case Lit(Literal.Bool(false, _), _, _) => eval(exp2, env0, k)
        // Case 3: exp1 is residual. Partially evaluate exp2.
        case _ => eval(exp2, env0, e2 => e2 match {
          // Case 3.1: exp2 is true. The result is true.
          case Lit(Literal.Bool(true, loc), tpe, _) => k(Lit(Literal.Bool(lit = true, loc), tpe, loc))
          // Case 3.2: exp2 is false or residual. Reconstruct the term. // TODO:  This seems incorrect
          case _ => exp.copy(exp1 = e1, exp2 = e2)
        })
      })

    case exp@Binary(BinaryOperator.And, exp1, exp2, _, _) =>
      // partially evaluate exp1
      eval(exp1, env0, e1 => e1 match {
        // Case 1: exp1 is false. The result is false.
        case Lit(Literal.Bool(false, loc), tpe, _) => k(Lit(Literal.Bool(lit = false, loc), tpe, loc))
        // Case 2: exp1 is true. The result is exp2.
        case Lit(Literal.Bool(true, loc), tpe, _) => eval(exp2, env0, k)
        // Case 3: exp1 is residual. Partially evaluate exp2.
        case _ => ???
      })

  }

}
