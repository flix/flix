package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.UnaryOperator.LogicalNot
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, UnaryOperator}
import ca.uwaterloo.flix.runtime.Interpreter.InternalRuntimeError

object PartialEvaluator {

  /**
    * The type of the continuation used by eval.
    */
  type Cont = Expression => Expression

  /**
    * The type of the continuation used by eval2.
    */
  type Cont2 = (Expression, Expression) => Expression

  /**
    * Partially evaluates the given expression `exp0` under the given environment `env0`
    *
    * Returns the residual expression.
    */
  def eval(exp0: Expression, root: SimplifiedAst.Root, env0: Map[String, Expression]): Expression = {

    /**
      * Partially evaluates the given expression `exp0` under the given environment `env0`.
      *
      * Applies the continuation `k` to the result of the evaluation.
      */
    def eval(exp0: Expression, env0: Map[String, Expression], k: Cont): Expression = exp0 match {
      /**
        * Unit Expression.
        */
      case Unit => k(exp0)

      /**
        * True Expression.
        */
      case True => k(True)

      /**
        * False Expression.
        */
      case False => k(False)

      /**
        * Int Expressions.
        */
      case Int8(lit) => k(Int8(lit))
      case Int16(lit) => k(Int16(lit))
      case Int32(lit) => k(Int32(lit))
      case Int64(lit) => k(Int64(lit))

      // TODO
      case v: Closure => k(v)

      /**
        * Str Expression.
        */
      case Str(lit) => k(Str(lit))

      /**
        * Var Expressions/
        */
      case Var(name, offset, tpe, loc) => env0.get(name.name) match {
        case None => throw new InternalRuntimeError(s"Unresolved variable: '$name'.")
        case Some(e) => eval(e, env0, k)
      }

      /**
        * Ref Expressions.
        */
      case Ref(name, tpe, loc) => root.constants.get(name) match {
        case None => throw new InternalRuntimeError(s"Unresolved reference: '$name'.")
        case Some(defn) => k(defn.exp)
      }

      /**
        * Unary Expressions.
        */
      case Unary(op, exp, _, _) => op match {
        /**
          * Unary Logical Not.
          */
        case UnaryOperator.LogicalNot => eval(exp, env0, {
          case True => k(False)
          case False => k(True)
          case residual => k(residual)
        })

        /**
          * Unary Plus.
          */
        case UnaryOperator.Plus => eval(exp, env0, k)

        /**
          * Unary Minus.
          */
        case UnaryOperator.Minus => eval(exp, env0, {
          case Int8(i) => k(Int8(byte(-i)))
          case Int16(i) => k(Int16(short(-i)))
          case Int32(i) => k(Int32(-i))
          case Int64(i) => k(Int64(-i))
          case residual => k(residual)
        })

        /**
          * Unary Bitwise Negation.
          */
        case UnaryOperator.BitwiseNegate => eval(exp, env0, {
          case Int8(i) => k(Int8(byte(~i)))
          case Int16(i) => k(Int16(short(~i)))
          case Int32(i) => k(Int32(~i))
          case Int64(i) => k(Int64(~i))
          case residual => k(residual)
        })
      }

      /**
        * Binary Expressions.
        */
      case Binary(op, exp1, exp2, tpe, loc) => op match {

        /**
          * Arithmetic Addition.
          */
        case BinaryOperator.Plus =>
          // Partially evaluate exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x + y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x + y)))
            case (Int32(x), Int32(y)) => k(Int32(x + y))
            case (Int64(x), Int64(y)) => k(Int64(x + y))

            // Identity laws.
            case (Int8(0), y) => k(y)
            case (Int16(0), y) => k(y)
            case (Int32(0), y) => k(y)
            case (Int64(0), y) => k(y)

            case (x, Int8(0)) => k(x)
            case (x, Int16(0)) => k(x)
            case (x, Int32(0)) => k(x)
            case (x, Int64(0)) => k(x)

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        case BinaryOperator.Minus => ??? // TODO
        case BinaryOperator.Times => ??? // TODO
        case BinaryOperator.Divide => ??? // TODO
        case BinaryOperator.Modulo => ??? // TODO

        case BinaryOperator.Less => ??? // TODO
        case BinaryOperator.LessEqual => ??? // TODO
        case BinaryOperator.Greater => ??? // TODO
        case BinaryOperator.GreaterEqual => ??? // TODO

        /**
          * Equal.
          */
        case BinaryOperator.Equal =>
          // Partially evaluate exp1.
          eval(exp1, env0, {
            case e1 =>
              // Partially evaluate exp2.
              eval(exp2, env0, {
                case e2 => syntacticEqual(e1, e2, env0) match {
                  case Eq.Equal => k(True)
                  case Eq.NotEq => k(False)
                  case Eq.Unknown => k(Binary(op, e1, e2, tpe, loc))
                }
              })
          })

        /**
          * Not Equal.
          */
        case BinaryOperator.NotEqual =>
          k(Unary(UnaryOperator.LogicalNot, Binary(BinaryOperator.Equal, exp1, exp2, tpe, loc), tpe, loc))

        /**
          * LogicalOr.
          */
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

        /**
          * LogicalAnd.
          */
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

        /**
          * Logical Implication.
          */
        case BinaryOperator.Implication =>
          // Rewrite and partially evaluate the result.
          k(Binary(BinaryOperator.LogicalOr, Unary(LogicalNot, exp1, tpe, loc), exp2, tpe, loc))

        /**
          * Logical Biconditional.
          */
        case BinaryOperator.Biconditional =>
          // Rewrite and partially evaluate the result.
          k(Binary(BinaryOperator.LogicalAnd,
            Binary(BinaryOperator.Implication, exp1, exp2, tpe, loc),
            Binary(BinaryOperator.Implication, exp2, exp1, tpe, loc),
            tpe, loc))

        case BinaryOperator.BitwiseAnd => ??? // TODO
        case BinaryOperator.BitwiseOr => ??? // TODO
        case BinaryOperator.BitwiseXor => ??? // TODO
        case BinaryOperator.BitwiseLeftShift => ??? // TODO
        case BinaryOperator.BitwiseRightShift => ??? // TODO

      }

      /**
        * Let Expressions.
        */
      case Let(name, offset, exp1, exp2, tpe, loc) =>
        // Partially evaluate the bound value exp1.
        eval(exp1, env0, {
          case e if isValue(e) =>
            // Case 1: The bound value expression exp1 is a value.
            // Extend the environment and evaluate the body expression exp2.
            eval(exp2, env0 + (name.name -> e), k)
          case r =>
            println(r)
            ???
        })

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
      case Apply3(lambda, actuals, tpe, loc) =>
        // Partially evaluate the lambda expression.
        eval(lambda, env0, {
          case Lambda(_, formals, body, _, _) =>
            // Case 1: The application expression is a lambda abstraction.
            // Match the formals with the actuals.
            // TODO: This should probably evaluate each parameter before swapping it in?
            val env1 = (formals zip actuals).foldLeft(env0) {
              case (env, (formal, actual)) => env + (formal.ident.name -> actual)
            }
            // And evaluate the body expression.
            eval(body, env1, k)
          case Closure(formals, body, env1, _, _) =>
            // Case 2: The lambda expression is a closure.
            // Match the formals with the actuals.
            val env2 = (formals zip actuals).foldLeft(env1) {
              case (env, (formal, actual)) => env + (formal.ident.name -> actual)
            }

            // And evaluate the body expression.
            eval(body, env2, k)
          case r1 =>
            // Case 3: The lambda expression is residual.
            // Partially evaluate the arguments and (re)-construct the residual.
            println(exp0)
            println(exp0.tpe)
            println(env0)
            ???
        })

      /**
        * Lambda Expressions.
        */
      case Lambda(ann, args, body, tpe, loc) =>
        k(Closure(args, body, env0, tpe, loc))

      /**
        * Tag Expressions.
        */
      case CheckTag(tag, exp, loc) =>
        // Partially evaluate the nested expression exp.
        eval(exp, env0, {
          case Tag(_, tag1, exp1, _, _) =>
            // Case 1: The nested expression is a tag. Perform the tag check.
            if (tag.name == tag1.name)
            // Case 1.1: The tags are the same. Return true.
              k(True)
            else
            // Case 1.2: The tags are different. Return false.
              k(False)
          case r =>
            // Case 2: The nested value is residual (re)-construct the expression.
            ???
        })

      case GetTagValue(exp, tpe, loc) =>
        // Partially evaluate exp.
        eval(exp, env0, {
          case Tag(_, _, e, _, _) =>
            // Case 1: The expression exp evaluates to a tag.
            // The result is inner expression
            k(e)
          case r =>
            // Case 2: The expression is residual. Reconstruct the expression.
            k(GetTagValue(r, tpe, loc))
        })

      case Tag(enum, tag, exp1, tpe, loc) =>
        eval(exp1, env0, {
          case e1 => k(Tag(enum, tag, e1, tpe, loc))
        })

      /**
        * Tuple Expressions.
        */
      case GetTupleIndex(exp, offset, tpe, loc) =>
        // TODO: deal with tuple before recursing.
        // Partially evaluate the tuple expression exp.
        eval(exp, env0, {
          case Tuple(elms, _, _) =>
            // Case 1: The tuple expression is a tuple. Project the component.
            k(elms(offset))
          case r =>
            // Case 2: The tuple expression is residual. Reconstruct the expression.
            println(r)
            ??? // TODO
        })

      case Tuple(elms, tpe, loc) =>
        // TODO: Use fold with continuation
        elms match {
          case List(exp1, exp2) =>
            eval(exp1, env0, {
              case e1 => eval(exp2, env0, {
                case e2 => k(Tuple(List(e1, e2), tpe, loc))
              })
            })
          case _ => ???
        }


      case Set(elms, tpe, loc) => ??? // TODO

      /**
        * Error Expressions.
        */
      case Error(tpe, loc) => k(Error(tpe, loc))
      case MatchError(tpe, loc) => k(MatchError(tpe, loc))


      case Apply(_, _, _, _) => ??? // TODO: To be eliminated from this phase.
      case o: LoadBool => ??? // TODO: To be eliminated from this phase.
      case o: LoadInt8 => ??? // TODO: To be eliminated from this phase.
      case o: LoadInt16 => ??? // TODO: To be eliminated from this phase.
      case o: LoadInt32 => ??? // TODO: To be eliminated from this phase.
      case o: StoreBool => ??? // TODO: To be eliminated from this phase.
      case o: StoreInt8 => ??? // TODO: To be eliminated from this phase.
      case o: StoreInt16 => ??? // TODO: To be eliminated from this phase.
      case o: StoreInt32 => ??? // TODO: To be eliminated from this phase.
    }

    /**
      * Overloaded eval that partially evaluates the two arguments `exp1` and `exp2` under the environment `env0`.
      */
    def eval2(exp1: Expression, exp2: Expression, env0: Map[String, Expression], k: Cont2): Expression =
      eval(exp1, env0, {
        case e1 => eval(exp2, env0, {
          case e2 => k(e1, e2)
        })
      })

    eval(exp0, env0, x => x)
  }

  /**
    * Returns `true` iff the given expression `e` is a value.
    */
  private def isValue(e: Expression): Boolean = e match {
    case Unit => true
    case True => true
    case False => true
    case v: Int8 => true
    case v: Int16 => true
    case v: Int32 => true
    case v: Int64 => true
    case v: Str => true
    case v: Tag => isValue(v.exp)
    case v: Tuple => v.elms.forall(isValue)
    case v: Closure => true
    case _ => false
  }


  /**
    * A common super-type for the result of an equality comparison.
    */
  sealed trait Eq

  object Eq {

    /**
      * The two expressions must evaluate the same value.
      */
    case object Equal extends Eq

    /**
      * The two expressions must not evaluate to the same value.
      */
    case object NotEq extends Eq

    /**
      * It is unknown whether the two expressions evaluate to the same value.
      */
    case object Unknown extends Eq

  }

  /**
    * Returns an `Eq` result depending on whether the two expressions
    * `exp1` and `exp2` can evaluate to the same value.
    */
  private def syntacticEqual(exp1: Expression, exp2: Expression, env0: Map[String, Expression]): Eq =
    if (mustBeEqual(exp1, exp2, env0))
      Eq.Equal
    else if (mustNotBeEqual(exp1, exp2, env0))
      Eq.NotEq
    else
      Eq.Unknown

  /**
    * Returns `true` iff `exp1` and `exp2` *must* evaluate to the same value under the given environment `env0`.
    */
  private def mustBeEqual(exp1: Expression, exp2: Expression, env0: Map[String, Expression]): Boolean = (exp1, exp2) match {
    case (Unit, Unit) => true
    case (True, True) => true
    case (False, False) => true
    case (Tag(_, tag1, e1, _, _), Tag(_, tag2, e2, _, _)) =>
      tag1.name == tag2.name && mustBeEqual(e1, e2, env0)
    case (Tuple(elms1, _, _), Tuple(elms2, _, _)) => (elms1 zip elms2) forall {
      case (e1, e2) => mustBeEqual(e1, e2, env0)
    }
    //case _ => false
  }

  /**
    * Returns `true` iff `exp1` and `exp2` *cannot* evaluate to the same value under the given environment `env0`.
    */
  private def mustNotBeEqual(exp1: Expression, exp2: Expression, env0: Map[String, Expression]): Boolean = (exp1, exp2) match {
    case (Unit, Unit) => false
    case (True, False) => true
    case (False, True) => true
    case (Tag(_, tag1, e1, _, _), Tag(_, tag2, e2, _, _)) =>
      tag1.name != tag2.name || mustNotBeEqual(e1, e2, env0)
    case (Tuple(elms1, _, _), Tuple(elms2, _, _)) => (elms1 zip elms2) exists {
      case (e1, e2) => mustNotBeEqual(e1, e2, env0)
    }
  }

  /**
    * Returns the canonical form the given expression `e`.
    *
    * A canonical form is a standard way to represent a class of expressions.
    *
    * For example, the following expressions are all equivalent:
    * - x > 0
    * - 0 < x
    * - x >= 0 && x != 0
    * - 0 <= x && x != 0
    *
    * This function attempts to pick a "standard form" of such an expression.
    */
  private def canonical(e: Expression): Expression = e match {
    case Unit => Unit
  }

  /**
    * Short-hand for casting an Int to a Byte.
    */
  private def byte(i: Int): Byte = i.asInstanceOf[Byte]

  /**
    * Short-hand for casting an Int to a Short.
    */
  private def short(i: Int): Short = i.asInstanceOf[Short]

  // http://www.lshift.net/blog/2007/06/11/folds-and-continuation-passing-style/

  //  Here’s the direct-style left-fold function:
  //
  //    (define (foldl kons knil xs)
  //  (if (null? xs)
  //    knil
  //    (foldl kons (kons (car xs) knil) (cdr xs))))
  //  and here’s the continuation-passing left-fold function:
  //
  //    (define (foldl-k kons knil xs k)
  //  (if (null? xs)
  //    (k knil)
  //      (kons (car xs) knil (lambda (v) (foldl-k kons v (cdr xs) k)))))
  //  Note that kons takes three arguments here, where in the direct-style version, it takes two.
  //
  //    One benefit of having CPS folds available is that they expose more control over the loop. For instance, using a normal fold, there’s no way to terminate the iteration early, but using a CPS fold, your three-argument kons routine can simply omit invoking its continuation parameter (presumably choosing some other continuation to run instead). This means that operations like (short-circuiting) contains?, any, and every can be written with CPS fold, but not with plain direct-style fold:
  //
  //    (define (contains? predicate val elements)
  //  (foldl-k (lambda (elt acc k)
  //  (if (predicate elt val)
  //  #t ;; note: skips the offered continuation!
  //  (k acc)))
  //  #f
  //  elements
  //  (lambda (v) v)))
}
