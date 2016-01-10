package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{BinaryOperator, UnaryOperator}
import ca.uwaterloo.flix.language.backend.ir.ExecutableAst.Expression
import ca.uwaterloo.flix.language.backend.ir.ExecutableAst.Expression._
import ca.uwaterloo.flix.language.backend.ir.{ExecutableAst, ReducedIR}

object PartialEvaluator {

  /**
    * The type of the environment used by the partial evaluator.
    */
  type Env = Map[Int, ReducedIR.Expression]

  /**
    * The type of the continuation used by the partial evaluator.
    */
  type Cont = ExecutableAst.Expression => ExecutableAst.Expression

  /**
    * Partially evaluates the given expression `exp0` under the given environment `env0`.
    *
    * Applies the continuation `k` to the result of the evaluation.
    */
  def eval(exp0: Expression, env0: Env, k: Cont): Expression = exp0 match {
    /*
      * Constant Expressions.
      */
    case Unit => k(exp0)

    /*
      * Unary Expressions.
      */
    case Unary(op, exp, _, _) => op match {
      case UnaryOperator.Not => eval(exp0, env0, {
        case True => k(False)
        case False => k(True)
        case residual => k(residual)
      })

      case UnaryOperator.Plus => eval(exp, env0, k)

      case UnaryOperator.Minus => eval(exp, env0, {
        case Int(i) => k(Int(-i))
        case residual => k(residual)
      })

      case UnaryOperator.Negate => eval(exp, env0, {
        case Int(i) => Int(~i)
        case residual => k(residual)
      })
    }

    /*
      * Binary Expressions.
      */
    case Binary(op, exp1, exp2, _, _) => op match {
      case BinaryOperator.Or =>
        ???

      case BinaryOperator.And =>
        ???
    }
//
//
//    /*
//      * If-then-else Expressions.
//      */
//
//    case exp@Binary(BinaryOperator.Or, exp1, exp2, _, _) =>
//      // partially evaluate exp1
//      eval(exp1, env0, e1 => e1 match {
//        // Case 1: exp1 is true. The result is true.
//        case Lit(Literal.Bool(true, loc), tpe, _) => k(True(loc))
//        // Case 2: exp1 is false. The result is exp2.
//        case Lit(Literal.Bool(false, _), _, _) => eval(exp2, env0, k)
//        // Case 3: exp1 is residual. Partially evaluate exp2.
//        case _ => eval(exp2, env0, e2 => e2 match {
//          // Case 3.1: exp2 is true. The result is true.
//          case Lit(Literal.Bool(true, loc), tpe, _) => k(Lit(Literal.Bool(lit = true, loc), tpe, loc))
//          // Case 3.2: exp2 is false or residual. Reconstruct the term. // TODO:  This seems incorrect
//          case _ => exp.copy(exp1 = e1, exp2 = e2)
//        })
//      })
//
//    case exp@Binary(BinaryOperator.And, exp1, exp2, _, _) =>
//      // partially evaluate exp1
//      eval(exp1, env0, e1 => e1 match {
//        // Case 1: exp1 is false. The result is false.
//        case False(loc) => k(Lit(Literal.Bool(lit = false, loc), tpe, loc))
//        // Case 2: exp1 is true. The result is exp2.
//        case True(loc) => eval(exp2, env0, k)
//        // Case 3: exp1 is residual. Partially evaluate exp2.
//        case _ => ???
//      })

  }

}
