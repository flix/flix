package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.Compiler.InternalCompilerError
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast.{BinaryOperator, SimplifiedAst, Type, UnaryOperator}

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
    * The type of the continuation used by evaln.
    */
  type ContN = List[Expression] => Expression

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

      /**
        * Closure Expressions.
        */
      case v: Closure => k(v)

      /**
        * Str Expression.
        */
      case Str(lit) => k(Str(lit))

      /**
        * Var Expressions/
        */
      case Var(name, offset, tpe, loc) => env0.get(name.name) match {
        case None => throw new InternalCompilerError(s"Unresolved variable: '$name'.")
        case Some(e) => eval(e, env0, k)
      }

      /**
        * Ref Expressions.
        */
      case Ref(name, tpe, loc) => root.constants.get(name) match {
        case None => throw new InternalCompilerError(s"Unresolved reference: '$name'.")
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
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x + y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x + y)))
            case (Int32(x), Int32(y)) => k(Int32(x + y))
            case (Int64(x), Int64(y)) => k(Int64(x + y))

            // Identity Laws: Addition by One.
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

        /**
          * Arithmetic Subtraction.
          */
        case BinaryOperator.Minus =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x - y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x - y)))
            case (Int32(x), Int32(y)) => k(Int32(x - y))
            case (Int64(x), Int64(y)) => k(Int64(x - y))

            // Identity Laws: Subtraction by One.
            case (x, Int8(0)) => k(x)
            case (x, Int16(0)) => k(x)
            case (x, Int32(0)) => k(x)
            case (x, Int64(0)) => k(x)

            // Residuals.
            case (r1, r2) =>
              // Equality Law: x - x = 0
              isEq(r1, r2, env0) match {
                case Eq.Equal => tpe match {
                  case Type.Int8 => k(Int8(0))
                  case Type.Int16 => k(Int16(0))
                  case Type.Int32 => k(Int32(0))
                  case Type.Int64 => k(Int64(0))
                  case _ => throw new InternalCompilerError(s"Illegal type: '$tpe'.")
                }
                case _ => k(Binary(op, r1, r2, tpe, loc))
              }
          })

        /**
          * Arithmetic Multiplication.
          */
        case BinaryOperator.Times =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x * y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x * y)))
            case (Int32(x), Int32(y)) => k(Int32(x * y))
            case (Int64(x), Int64(y)) => k(Int64(x * y))

            // Identity Laws: Multiplication by Zero.
            case (Int8(0), _) => k(Int8(0))
            case (Int16(0), _) => k(Int16(0))
            case (Int32(0), _) => k(Int32(0))
            case (Int64(0), _) => k(Int64(0))
            case (_, Int8(0)) => k(Int8(0))
            case (_, Int16(0)) => k(Int16(0))
            case (_, Int32(0)) => k(Int32(0))
            case (_, Int64(0)) => k(Int64(0))

            // Identity Laws: Multiplication by One.
            case (Int8(1), y) => k(y)
            case (Int16(1), y) => k(y)
            case (Int32(1), y) => k(y)
            case (Int64(1), y) => k(y)
            case (x, Int8(1)) => k(x)
            case (x, Int16(1)) => k(x)
            case (x, Int32(1)) => k(x)
            case (x, Int64(1)) => k(x)

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Arithmetic Division.
          */
        case BinaryOperator.Divide =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) if y != 0 => k(Int8(byte(x / y)))
            case (Int16(x), Int16(y)) if y != 0 => k(Int16(short(x / y)))
            case (Int32(x), Int32(y)) if y != 0 => k(Int32(x / y))
            case (Int64(x), Int64(y)) if y != 0 => k(Int64(x / y))

            // Identity Laws: Division by One.
            case (x, Int8(1)) => k(x)
            case (x, Int16(1)) => k(x)
            case (x, Int32(1)) => k(x)
            case (x, Int64(1)) => k(x)

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Arithmetic Modulus.
          */
        case BinaryOperator.Modulo =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) if y != 0 => k(Int8(byte(x % y)))
            case (Int16(x), Int16(y)) if y != 0 => k(Int16(short(x % y)))
            case (Int32(x), Int32(y)) if y != 0 => k(Int32(x % y))
            case (Int64(x), Int64(y)) if y != 0 => k(Int64(x % y))

            // Identity Laws: Modulus by One.
            case (x, Int8(1)) => k(Int8(0))
            case (x, Int16(1)) => k(Int16(0))
            case (x, Int32(1)) => k(Int32(0))
            case (x, Int64(1)) => k(Int64(0))

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Less-than.
          */
        case BinaryOperator.Less =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => if (x < y) k(True) else k(False)
            case (Int16(x), Int16(y)) => if (x < y) k(True) else k(False)
            case (Int32(x), Int32(y)) => if (x < y) k(True) else k(False)
            case (Int64(x), Int64(y)) => if (x < y) k(True) else k(False)

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Less-than or equal.
          */
        case BinaryOperator.LessEqual =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            case (e1, e2) =>
              k(Binary(BinaryOperator.LogicalOr,
                Binary(BinaryOperator.Less, e1, e2, Type.Bool, loc),
                Binary(BinaryOperator.Equal, e1, e2, Type.Bool, loc),
                Type.Bool, loc
              ))
          })

        /**
          * Greater-than.
          */
        case BinaryOperator.Greater =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => if (x > y) k(True) else k(False)
            case (Int16(x), Int16(y)) => if (x > y) k(True) else k(False)
            case (Int32(x), Int32(y)) => if (x > y) k(True) else k(False)
            case (Int64(x), Int64(y)) => if (x > y) k(True) else k(False)

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Greater-than or equal.
          */
        case BinaryOperator.GreaterEqual =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            case (e1, e2) =>
              k(Binary(BinaryOperator.LogicalOr,
                Binary(BinaryOperator.Greater, e1, e2, Type.Bool, loc),
                Binary(BinaryOperator.Equal, e1, e2, Type.Bool, loc),
                Type.Bool, loc
              ))
          })

        /**
          * Equal.
          */
        case BinaryOperator.Equal =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            case (e1, e2) => isEq(e1, e2, env0) match {
              case Eq.Equal => k(True)
              case Eq.NotEq => k(False)
              case Eq.Unknown => k(Binary(op, e1, e2, tpe, loc))
            }
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
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            case (True, _) => k(True)
            case (_, True) => k(True)
            case (False, False) => k(False)
            case (r1, r2) => k(Binary(BinaryOperator.LogicalOr, r1, r2, tpe, loc))
          })

        /**
          * LogicalAnd.
          */
        case BinaryOperator.LogicalAnd =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            case (True, True) => k(True)
            case (False, _) => k(False)
            case (_, False) => k(False)
            case (r1, r2) => k(Binary(BinaryOperator.LogicalAnd, r1, r2, tpe, loc))
          })

        /**
          * Logical Implication.
          */
        case BinaryOperator.Implication =>
          // Rewrite and partially evaluate the result.
          k(Binary(BinaryOperator.LogicalOr, Unary(UnaryOperator.LogicalNot, exp1, tpe, loc), exp2, tpe, loc))

        /**
          * Logical Biconditional.
          */
        case BinaryOperator.Biconditional =>
          // Rewrite and partially evaluate the result.
          k(Binary(BinaryOperator.LogicalAnd,
            Binary(BinaryOperator.Implication, exp1, exp2, tpe, loc),
            Binary(BinaryOperator.Implication, exp2, exp1, tpe, loc),
            tpe, loc))

        /**
          * Bitwise And.
          */
        case BinaryOperator.BitwiseAnd =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x & y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x & y)))
            case (Int32(x), Int32(y)) => k(Int32(x & y))
            case (Int64(x), Int64(y)) => k(Int64(x & y))

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Bitwise Or.
          */
        case BinaryOperator.BitwiseOr =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x | y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x | y)))
            case (Int32(x), Int32(y)) => k(Int32(x | y))
            case (Int64(x), Int64(y)) => k(Int64(x | y))

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Bitwise Xor.
          */
        case BinaryOperator.BitwiseXor =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x ^ y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x ^ y)))
            case (Int32(x), Int32(y)) => k(Int32(x ^ y))
            case (Int64(x), Int64(y)) => k(Int64(x ^ y))

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Bitwise Left Shift.
          */
        case BinaryOperator.BitwiseLeftShift =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x << y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x << y)))
            case (Int32(x), Int32(y)) => k(Int32(x << y))
            case (Int64(x), Int64(y)) => k(Int64(x << y))

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Bitwise Right Shift.
          */
        case BinaryOperator.BitwiseRightShift =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, env0, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x >> y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x >> y)))
            case (Int32(x), Int32(y)) => k(Int32(x >> y))
            case (Int64(x), Int64(y)) => k(Int64(x >> y))

            // Reconstruction
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })
      }

      /**
        * Let Expressions.
        */
      case Let(name, offset, exp1, exp2, tpe, loc) =>
        // Partially evaluate the bound value exp1.
        eval(exp1, env0, {
          case e if isValue(e) => // TODO: Why?
            // Case 1: The bound value expression exp1 is a value.
            // Extend the environment and evaluate the body expression exp2.
            eval(exp2, env0 + (name.name -> e), k) // TODO: Carefull with substi.
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
        // TODO: Carefull with substi.
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
      case Tag(enum, tag, exp1, tpe, loc) =>
        eval(exp1, env0, {
          case e1 => k(Tag(enum, tag, e1, tpe, loc))
        })

      /**
        * CheckTag Expressions.
        */
      case CheckTag(tag1, exp, loc) =>
        eval(exp, env0, {
          case Tag(_, tag2, _, _, _) =>
            if (tag1.name == tag2.name)
              k(True)
            else
              k(False)
          case r => k(CheckTag(tag1, r, loc))
        })

      /**
        * GetTagValue Expression.
        */
      case GetTagValue(exp, tpe, loc) =>
        eval(exp, env0, {
          case Tag(_, _, e, _, _) => k(e)
          case r => k(GetTagValue(r, tpe, loc))
        })

      /**
        * Tuple Expressions.
        */
      case Tuple(elms, tpe, loc) =>
        evaln(elms, env0, {
          case xs => k(Tuple(xs, tpe, loc))
        })

      /**
        * GetTupleIndex Expressions.
        */
      case GetTupleIndex(exp, offset, tpe, loc) =>
        eval(exp, env0, {
          case Tuple(elms, _, _) => k(elms(offset))
          case r => GetTupleIndex(r, offset, tpe, loc)
        })

      /**
        * Error Expressions.
        */
      case Error(tpe, loc) => k(Error(tpe, loc))

      /**
        * Match Error Expressions.
        */
      case MatchError(tpe, loc) => k(MatchError(tpe, loc))

      // TODO: Unsupported
      case Set(elms, tpe, loc) => throw new InternalCompilerError("Not Yet Supported. Sorry.")
      case o: CheckNil => throw new InternalCompilerError("Not Yet Supported. Sorry.")
      case o: CheckCons => throw new InternalCompilerError("Not Yet Supported. Sorry.")

      // TODO: This will be eliminated.
      case Apply(_, _, _, _) => ???

      // TODO: These will be moved to a different AST.
      case o: LoadBool => throw new InternalCompilerError("Unsupported.")
      case o: LoadInt8 => throw new InternalCompilerError("Unsupported.")
      case o: LoadInt16 => throw new InternalCompilerError("Unsupported.")
      case o: LoadInt32 => throw new InternalCompilerError("Unsupported.")
      case o: StoreBool => throw new InternalCompilerError("Unsupported.")
      case o: StoreInt8 => throw new InternalCompilerError("Unsupported.")
      case o: StoreInt16 => throw new InternalCompilerError("Unsupported.")
      case o: StoreInt32 => throw new InternalCompilerError("Unsupported.")

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

    /**
      * Overloaded eval that partially evaluates the given list of expressions `exps` under the environment `env0`.
      */
    def evaln(exps: List[Expression], env0: Map[String, Expression], k: ContN): Expression = {
      def visit(es: List[Expression], rs: List[Expression]): Expression = es match {
        case Nil => k(rs.reverse)
        case x :: xs => eval(x, env0, {
          case e => visit(xs, e :: rs)
        })
      }

      visit(exps, Nil)
    }

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
    * Compares the two expressions `exp1` and `exp2` under the environment `env0`.
    *
    * Returns `Eq.Equal` if evaluation of `exp1` and `exp2` is *guaranteed* to produce the *same* value.
    * Returns `Eq.NotEqual` if evaluation of `exp1` and `exp2` is *guaranteed* to produce *different* values.
    * Returns `Eq.Unknown` if the procedure cannot determine whether the two expressions may or may not produce the same value.
    */
  private def isEq(exp1: Expression, exp2: Expression, env0: Map[String, Expression]): Eq =
    if (mustEq(exp1, exp2, env0))
      Eq.Equal
    else if (mustNotEq(exp1, exp2, env0))
      Eq.NotEq
    else
      Eq.Unknown

  /**
    * Returns `true` iff `exp1` and `exp2` *must* evaluate to the same value under the given environment `env0`.
    */
  // TODO: Implement rest
  private def mustEq(exp1: Expression, exp2: Expression, env0: Map[String, Expression]): Boolean = (exp1, exp2) match {
    case (Unit, Unit) => true
    case (True, True) => true
    case (False, False) => true
    case (Int8(i1), Int8(i2)) => i1 == i2
    case (Int16(i1), Int16(i2)) => i1 == i2
    case (Int32(i1), Int32(i2)) => i1 == i2
    case (Int64(i1), Int64(i2)) => i1 == i2
    case (Str(s1), Str(s2)) => s1 == s2
    case (Var(ident1, _, _, _), Var(ident2, _, _, _)) =>
      val e1 = env0(ident1.name)
      val e2 = env0(ident2.name)
      mustEq(e1, e2, env0)
    case (Ref(name1, _, _), Ref(name2, _, _)) =>
      name1.fqn == name2.fqn
    case (Lambda(_, _, body1, _, _), Lambda(_, _, body2, _, _)) =>
      mustEq(body1, body2, env0)
    case (Apply3(lambda1, actuals1, _, _), Apply3(lambda2, actuals2, _, _)) =>
      val eqLambdas = mustEq(lambda1, lambda2, env0)
      val eqActuals = (actuals1 zip actuals2).forall {
        case (e1, e2) => mustEq(e1, e2, env0)
      }
      eqLambdas && eqActuals
    case (Unary(op1, e1, _, _), Unary(op2, e2, _, _)) =>
      val eqOp = op1 == op2
      val eqExp = mustEq(e1, e2, env0)
      eqOp && eqExp
    case (Binary(op1, e11, e12, _, _), Binary(op2, e21, e22, _, _)) =>
      val eqOp = op1 == op2
      val eq1Exp = mustEq(e11, e21, env0)
      val eq2Exp = mustEq(e12, e22, env0)
      eqOp && eq1Exp && eq2Exp
    case (IfThenElse(e11, e12, e13, _, _), IfThenElse(e21, e22, e23, _, _)) =>
      val eq1Exp = mustEq(e11, e21, env0)
      val eq2Exp = mustEq(e12, e22, env0)
      val eq3Exp = mustEq(e13, e23, env0)
      eq1Exp && eq2Exp && eq3Exp
    case (Let(ident1, _, e11, e12, _, _), Let(ident2, _, e21, e22, _, _)) =>
      val eqIdent = ident1.name == ident2.name
      val eqExp1 = mustEq(e11, e21, env0)
      val eqExp2 = mustEq(e12, e22, env0)
      eqIdent && eqExp1 && eqExp2



    case (Tag(_, tag1, e1, _, _), Tag(_, tag2, e2, _, _)) =>
      tag1.name == tag2.name && mustEq(e1, e2, env0)
    case (Tuple(elms1, _, _), Tuple(elms2, _, _)) => (elms1 zip elms2) forall {
      case (e1, e2) => mustEq(e1, e2, env0)
    }
    //case _ => false
  }

  /**
    * Returns `true` iff `exp1` and `exp2` *cannot* evaluate to the same value under the given environment `env0`.
    */
  // TODO: Implement rest
  private def mustNotEq(exp1: Expression, exp2: Expression, env0: Map[String, Expression]): Boolean = (exp1, exp2) match {
    case (Unit, Unit) => false
    case (True, False) => true
    case (False, True) => true
    case (Tag(_, tag1, e1, _, _), Tag(_, tag2, e2, _, _)) =>
      tag1.name != tag2.name || mustNotEq(e1, e2, env0)
    case (Tuple(elms1, _, _), Tuple(elms2, _, _)) => (elms1 zip elms2) exists {
      case (e1, e2) => mustNotEq(e1, e2, env0)
    }
  }

  /**
    * Short-hand for casting an Int to a Byte.
    */
  private def byte(i: Int): Byte = i.asInstanceOf[Byte]

  /**
    * Short-hand for casting an Int to a Short.
    */
  private def short(i: Int): Short = i.asInstanceOf[Short]

}
