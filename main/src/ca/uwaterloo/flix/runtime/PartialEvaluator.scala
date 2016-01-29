package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, UnaryOperator}
import ca.uwaterloo.flix.runtime.Interpreter.InternalRuntimeError

object PartialEvaluator {

  /**
    * The type of the continuation used by the partial evaluator.
    */
  type Cont = Expression => Expression

  def eval(exp0: Expression, root: SimplifiedAst.Root, env0: Map[String, Expression]): Expression = {

    /**
      * Partially evaluates the given expression `exp0` under the given environment `env0`.
      *
      * Applies the continuation `k` to the result of the evaluation.
      */
    def eval(exp0: Expression, env0: Map[String, Expression], k: Cont): Expression = exp0 match {
      /**
        * Constant Expressions.
        */
      case Unit => k(exp0)

      /**
        * Ref Expressions.
        */
      case Ref(name, tpe, loc) => root.constants.get(name) match {
        case None => throw new InternalRuntimeError(s"Unknown name '$name'.")
        case Some(defn) => k(defn.exp)
      }

      /**
        * Unary Expressions.
        */
      case Unary(op, exp, _, _) => op match {
        case UnaryOperator.LogicalNot => eval(exp0, env0, {
          case True => k(False)
          case False => k(True)
          case residual => k(residual)
        })

        case UnaryOperator.Plus => eval(exp, env0, k)

        case UnaryOperator.Minus => eval(exp, env0, {
          case Int(i) => k(Int(-i))
          case residual => k(residual)
        })

        case UnaryOperator.BitwiseNegate => eval(exp, env0, {
          case Int(i) => Int(~i)
          case residual => k(residual)
        })
      }

      /**
        * Binary Expressions.
        */
      case Binary(op, exp1, exp2, tpe, loc) => op match {
        case BinaryOperator.LogicalOr =>
          // Partially evaluate exp1.
          eval(exp1, env0, {
            // Case 1: exp1 is true. The result is true.
            case True => k(True)
            // Case 2: exp1 is false. The result is exp2.
            case False => eval(exp2, env0, k)
            // Case 3: exp1 is residual. Partially evaluate exp2.
            case r1 => eval(exp2, env0, {
              // Case 3.1: exp2 is true. The result is true.
              case True => k(True)
              // Case 3.2: exp2 is false. The result is the exp1 (i.e. its residual).
              case False => k(r1)
              // Case 3.3: exp2 is also residual. The result is residual.
              case r2 => k(Binary(BinaryOperator.LogicalOr, r1, r2, tpe, loc))
            })
          })

        // TODO: Eq for boolean ops.

        case BinaryOperator.LogicalAnd =>
          // Partially evaluate exp1.
          eval(exp1, env0, {
            // Case 1: exp1 is true. The result is exp2.
            case True => eval(exp2, env0, k)
            // Case 2: exp1 is false. The result is false.
            case False => k(False)
            // Case 3: exp1 is residual. Partially evaluate exp2.
            case r1 => eval(exp2, env0, {
              // Case 3.1: exp2 is true. The result is exp1 (i.e. its residual).
              case True => k(r1)
              // Case 3.2: exp2 is false. The result is false.
              case False => k(False)
              // Case 3.3: exp3 is also residual. The result is residual.
              case r2 => k(Binary(BinaryOperator.LogicalAnd, r1, r2, tpe, loc))
            })
          })
      }

      /**
        * If-then-else Expressions.
        */
      case IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Partially evaluate exp1.
        eval(exp1, env0, {
          // Case 1: The condition is true. The result is exp2.
          case True => eval(exp2, env0, k)
          // Case 2: The condition is false. The result is exp3.
          case False => eval(exp3, env0, k)
          // Case 3: The condition is residual.
          // Partially evaluate exp2 and exp3 and (re-)construct the residual.
          case r1 => eval(exp2, env0, {
            case r2 => eval(exp3, env0, {
              case r3 => k(IfThenElse(r1, r2, r3, tpe, loc))
            })
          })
        })

      /**
        * Apply Expressions.
        */
      case Apply3(e0, actuals, tpe, loc) =>
        // Partially evaluate e.
        eval(e0, env0, {
          case Lambda(annotations, formals, body, _, _) =>
            // Case 1: The application expression is a lambda abstraction.
            // Match the formals with the actuals.
            // TODO: This should probably evaluate each parameter before swapping it in?
            val env1 = (formals zip actuals).foldLeft(env0) {
              case (env, (formal, actual)) => env + (formal.ident.name -> actual)
            }
            eval(body, env1, k)
          case r1 =>
            // Case 2: The application expression is residual.
            // Partially evaluate the arguments and (re)-construct the residual.
            ???
        })

    }

    eval(exp0, env0, x => x)
  }
}
