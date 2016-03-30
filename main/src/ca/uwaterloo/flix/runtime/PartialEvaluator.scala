package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression
import ca.uwaterloo.flix.language.ast.SimplifiedAst.Expression._
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase.GenSym
import ca.uwaterloo.flix.util.InternalCompilerException

// TODO: Use coverage to determine which of these are redundant.

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
  def eval(exp0: Expression, env0: Map[String, Expression], root: SimplifiedAst.Root)(implicit genSym: GenSym): Expression = {

    /**
      * Partially evaluates the given expression `exp0` under the given environment `env0`.
      *
      * Applies the continuation `k` to the result of the evaluation.
      */
    def eval(exp0: Expression, k: Cont): Expression = exp0 match {
      /**
        * Unit Expression.
        */
      case Unit => k(Unit)

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
        * Str Expression.
        */
      case Str(lit) => k(Str(lit))

      /**
        * Var Expressions/
        */
      case Var(name, offset, tpe, loc) => k(Var(name, offset, tpe, loc))

      /**
        * Ref Expressions.
        */
      case Ref(name, tpe, loc) => root.constants.get(name) match {
        case None => throw InternalCompilerException(s"Unresolved reference: '$name'.")
        case Some(defn) => k(defn.exp)
      }

      /**
        * Unary Expressions.
        */
      case Unary(op, exp, _, _) => op match {
        /**
          * Unary Logical Not.
          */
        case UnaryOperator.LogicalNot => eval(exp, {
          case True => k(False)
          case False => k(True)
          case residual => k(residual)
        })

        /**
          * Unary Plus.
          */
        case UnaryOperator.Plus => eval(exp, k)

        /**
          * Unary Minus.
          */
        case UnaryOperator.Minus => eval(exp, {
          case Int8(i) => k(Int8(byte(-i)))
          case Int16(i) => k(Int16(short(-i)))
          case Int32(i) => k(Int32(-i))
          case Int64(i) => k(Int64(-i))
          case residual => k(residual)
        })

        /**
          * Unary Bitwise Negation.
          */
        case UnaryOperator.BitwiseNegate => eval(exp, {
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
          eval2(exp1, exp2, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x + y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x + y)))
            case (Int32(x), Int32(y)) => k(Int32(x + y))
            case (Int64(x), Int64(y)) => k(Int64(x + y))

            // Identity Laws: Addition by Zero.
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
          eval2(exp1, exp2, {
            // Concrete execution.
            case (Int8(x), Int8(y)) => k(Int8(byte(x - y)))
            case (Int16(x), Int16(y)) => k(Int16(short(x - y)))
            case (Int32(x), Int32(y)) => k(Int32(x - y))
            case (Int64(x), Int64(y)) => k(Int64(x - y))

            // Identity Laws: Subtraction by Zero.
            case (x, Int8(0)) => k(x)
            case (x, Int16(0)) => k(x)
            case (x, Int32(0)) => k(x)
            case (x, Int64(0)) => k(x)

            // Residuals.
            case (r1, r2) => k(Binary(op, r1, r2, tpe, loc))
          })

        /**
          * Arithmetic Multiplication.
          */
        case BinaryOperator.Times =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
          // Rewrite.
          k(Binary(BinaryOperator.LogicalOr,
            Binary(BinaryOperator.Less, exp1, exp2, Type.Bool, loc),
            Binary(BinaryOperator.Equal, exp1, exp2, Type.Bool, loc),
            Type.Bool, loc
          ))


        /**
          * Greater-than.
          */
        case BinaryOperator.Greater =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, {
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
          // Rewrite.
          k(Binary(BinaryOperator.LogicalOr,
            Binary(BinaryOperator.Greater, exp1, exp2, Type.Bool, loc),
            Binary(BinaryOperator.Equal, exp1, exp2, Type.Bool, loc),
            Type.Bool, loc
          ))

        /**
          * Equal.
          */
        case BinaryOperator.Equal =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, {
            case (Var(ident1, _, _, _), Var(ident2, _, _, _)) if ident1.name == ident2.name =>
              // Case 1: The lhs and the rhs are the same variable.
              k(True)
            case (v1, v2) if isValue(v1) && isValue(v2) =>
              // Case 2: The lhs and the rhs are values.
              if (isEq(v1, v2))
                k(True)
              else
                k(False)
            case (Tag(_, tag1, e1, _, _), Tag(_, tag2, e2, _, _)) =>
              // Case 3: The lhs and rhs are tags.
              if (tag1.name != tag2.name)
                k(False)
              else
                eval(Binary(BinaryOperator.Equal, e1, e2, Type.Bool, loc), k)

            case (Tuple(elms1, _, _), Tuple(elms2, _, _)) =>
              // Case 4: The lhs and rhs are tuples. Rewrite the expression to compare each element.
              val conditions = (elms1 zip elms2) map {
                case (e1, e2) => Binary(BinaryOperator.Equal, e1, e2, Type.Bool, loc)
              }
              val result = conditions.foldRight(True: Expression) {
                case (cond, acc) => Binary(BinaryOperator.LogicalAnd, cond, acc, Type.Bool, loc)
              }
              eval(result, k)
            case (v1, IfThenElse(e1, e2, e3, _, _)) if isValue(v1) =>
              // Case 5: The lhs is a value and the rhs is an if-then-else.
              // Push the value inside the if-then-else expression.
              val condition = e1
              val consequence = Binary(BinaryOperator.Equal, v1, e2, Type.Bool, loc)
              val alternative = Binary(BinaryOperator.Equal, v1, e3, Type.Bool, loc)
              val ifthenelse = IfThenElse(condition, consequence, alternative, Type.Bool, loc)
              eval(ifthenelse, k)
            case (IfThenElse(e1, e2, e3, _, _), v2) if isValue(v2) =>
              // Case 6: The rhs is a value and the lhs is a value.
              // Push the value inside the if-then-else expression.
              val condition = e1
              val consequence = Binary(BinaryOperator.Equal, v2, e2, Type.Bool, loc)
              val alternative = Binary(BinaryOperator.Equal, v2, e3, Type.Bool, loc)
              val ifthenelse = IfThenElse(condition, consequence, alternative, Type.Bool, loc)
              eval(ifthenelse, k)
            case (IfThenElse(e11, e12, e13, _, _), IfThenElse(e21, e22, e23, _, _)) =>
              // Case 7: The lhs and rhs are both if-then-else.
              // Rewrite the expression from:
              //
              //   eq (
              //     if (e11) e12 else e13,
              //     if (e21) e22 else e23
              //   )
              //
              // into
              //
              //   if (e11)
              //       if (e21) eq(e12, e22) else eq(e12, e23)
              //     else
              //       if (e21) eq(e13, e22) else eq(e13, e23)
              //
              eval(IfThenElse(
                e11,
                IfThenElse(e21,
                  Binary(BinaryOperator.Equal, e12, e22, Type.Bool, loc),
                  Binary(BinaryOperator.Equal, e12, e23, Type.Bool, loc),
                  Type.Bool, loc),
                IfThenElse(e21,
                  Binary(BinaryOperator.Equal, e13, e22, Type.Bool, loc),
                  Binary(BinaryOperator.Equal, e13, e23, Type.Bool, loc),
                  Type.Bool, loc),
                Type.Bool,
                loc
              ), k)

            case (e1, e2) =>
              // Case 8: Reconstruct the expression.
              k(Binary(op, e1, e2, tpe, loc))
          })

        /**
          * Not Equal.
          */
        case BinaryOperator.NotEqual =>
          // Rewrite
          k(Unary(UnaryOperator.LogicalNot, Binary(BinaryOperator.Equal, exp1, exp2, tpe, loc), tpe, loc))

        /**
          * LogicalOr.
          */
        case BinaryOperator.LogicalOr =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, {
            case (True, _) => k(True)
            case (_, True) => k(True)
            case (False, False) => k(False)
            case (False, r2) => k(r2)
            case (r1, False) => k(r1)
            case (r1, r2) => k(Binary(BinaryOperator.LogicalOr, r1, r2, tpe, loc))
          })

        /**
          * LogicalAnd.
          */
        case BinaryOperator.LogicalAnd =>
          // Partially evaluate both exp1 and exp2.
          eval2(exp1, exp2, {
            case (True, True) => k(True)
            case (False, _) => k(False)
            case (_, False) => k(False)
            case (True, r2) => k(r2)
            case (r1, True) => k(r1)
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
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
          eval2(exp1, exp2, {
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
        * If-then-else Expressions.
        */
      case IfThenElse(exp1, exp2, exp3, tpe, loc) =>
        // Partially evaluate exp1.
        eval(exp1, {
          // Case 1: The condition is true. The result is exp2.
          case True => eval(exp2, k)
          // Case 2: The condition is false. The result is exp3.
          case False => eval(exp3, k)
          // Case 3: The condition is residual.
          // Partially evaluate exp2 and exp3 and (re-)construct the residual.
          case r1 => eval2(exp2, exp3, {
            // Case 3.1: Check if the entire if-then-else can be replaced by the conditional.
            case (True, False) =>
              k(r1)
            // Case 3.1: Check if the entire if-then-else can be replaced by the negated conditional.
            case (False, True) =>
              k(Unary(UnaryOperator.LogicalNot, r1, Type.Bool, loc))
            case (r2, r3) =>
              // Case 3.2: Check if the then and else expressions are equivalent.
              if (isValue(r2) && isValue(r3) && isEq(r2, r3))
                k(r2)
              else
                k(IfThenElse(r1, r2, r3, tpe, loc))
          })
        })

      /**
        * Let Expressions.
        */
      case Let(ident, offset, exp1, exp2, tpe, loc) =>
        // Partially evaluate the bound value exp1.
        eval(exp1, {
          case e =>
            eval(substitute(ident, e, exp2), k)
        })


      /**
        * Apply Expressions.
        */
      case Apply(lambda, args, tpe, loc) =>
        // Partially evaluate the argument expressions.
        evaln(args, {
          case actuals =>
            // Partially evaluate the lambda expression.
            eval(lambda, {
              case Lambda(formals, body, _, _) =>
                // Substitute actuals for formals.
                val result = (formals zip actuals).foldLeft(body) {
                  case (acc, (formal, actual)) =>
                    val src = formal.ident
                    val dst = actual
                    substitute(src, dst, acc)
                }
                // Evaluate the result body.
                eval(result, k)
              case r =>
                k(Apply(r, actuals, tpe, loc))
            })
        })

      /**
        * Lambda Expressions.
        */
      case Lambda(args, body, tpe, loc) =>
        k(Lambda(args, body, tpe, loc))

      /**
        * Hook Expressions.
        */
      case Hook(hook, tpe, loc) =>
        k(Hook(hook, tpe, loc))

      /**
        * Tag Expressions.
        */
      case Tag(enum, tag, exp1, tpe, loc) =>
        eval(exp1, {
          case e1 => k(Tag(enum, tag, e1, tpe, loc))
        })

      /**
        * CheckTag Expressions.
        */
      case CheckTag(tag1, exp, loc) =>
        eval(exp, {
          case Tag(_, tag2, _, _, _) =>
            // Case 1: Concrete execution.
            if (tag1.name == tag2.name)
              k(True)
            else
              k(False)
          case IfThenElse(e1, e2, e3, tpe, _) =>
            // Case 2: Move the CheckTag inside the consequence and alternative expressions.
            val conditional = e1
            val consequence = CheckTag(tag1, e2, loc)
            val alternative = CheckTag(tag1, e3, loc)
            val ifthenelse = IfThenElse(conditional, consequence, alternative, Type.Bool, loc)

            // Evaluate the rewritten if-then-else.
            eval(ifthenelse, k)
          case r =>
            // Case 3: The expression is residual. Reconstruct it.
            k(CheckTag(tag1, r, loc))
        })

      /**
        * GetTagValue Expression.
        */
      case GetTagValue(tag, exp, tpe, loc) =>
        eval(exp, {
          case Tag(_, _, e, _, _) =>
            // Case 1: Concrete execution.
            k(e)
          case IfThenElse(e1, e2, e3, _, _) =>
            // Case 2: Move the GetTagValue inside the consequence and alternative expressions.
            val conditional = e1
            val consequence = GetTagValue(tag, e2, tpe, loc)
            val alternative = GetTagValue(tag, e3, tpe, loc)
            val ifthenelse = IfThenElse(conditional, consequence, alternative, tpe, loc)

            // Evaluate the rewritten if-then-else.
            eval(ifthenelse, k)
          case r =>
            // Case 3: The expression is residual. Reconstruct it.
            k(GetTagValue(tag, r, tpe, loc))
        })

      /**
        * Tuple Expressions.
        */
      case Tuple(elms, tpe, loc) =>
        evaln(elms, {
          case xs => k(Tuple(xs, tpe, loc))
        })

      /**
        * GetTupleIndex Expressions.
        */
      case GetTupleIndex(exp, offset, tpe, loc) =>
        eval(exp, {
          case Tuple(elms, _, _) => k(elms(offset))
          case r => GetTupleIndex(r, offset, tpe, loc)
        })

      /**
        * Error Expressions.
        */
      case UserError(tpe, loc) => k(UserError(tpe, loc))

      /**
        * Match Error Expressions.
        */
      case MatchError(tpe, loc) => k(MatchError(tpe, loc))

      /**
        * Switch Error Expressions.
        */
      case SwitchError(tpe, loc) => k(SwitchError(tpe, loc))

      case FSet(elms, tpe, loc) => throw InternalCompilerException("Not Yet Supported. Sorry.")
      case o: CheckNil => throw InternalCompilerException("Not Yet Supported. Sorry.")
      case o: CheckCons => throw InternalCompilerException("Not Yet Supported. Sorry.")
      case ApplyRef(_, _, _, _) => throw InternalCompilerException("Deprecated.")
    }

    /**
      * Overloaded eval that partially evaluates the two arguments `exp1` and `exp2` under the environment `env0`.
      */
    def eval2(exp1: Expression, exp2: Expression, k: Cont2): Expression =
      eval(exp1, {
        case e1 => eval(exp2, {
          case e2 => k(e1, e2)
        })
      })

    /**
      * Overloaded eval that partially evaluates the given list of expressions `exps` under the environment `env0`.
      */
    def evaln(exps: List[Expression], k: ContN): Expression = {
      def visit(es: List[Expression], rs: List[Expression]): Expression = es match {
        case Nil => k(rs.reverse)
        case x :: xs => eval(x, {
          case e => visit(xs, e :: rs)
        })
      }

      visit(exps, Nil)
    }

    /**
      * The actual call to get things started.
      */
    val sexp = env0.foldLeft(exp0) {
      case (acc, (name, e)) =>
        val src = Name.Ident(SourcePosition.Unknown, name, SourcePosition.Unknown)
        val dst = e
        substitute(src, dst, acc)
    }
    eval(sexp, x => x)
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
    * Returns `true` if `exp1` is equal to `exp2`.
    *
    * The two arguments must be values.
    */
  private def isEq(exp1: Expression, exp2: Expression): Boolean = (exp1, exp2) match {
    case (Unit, Unit) => true
    case (True, True) => true
    case (False, False) => true
    case (True, False) => false
    case (False, True) => false
    case (Int8(i1), Int8(i2)) => i1 == i2
    case (Int16(i1), Int16(i2)) => i1 == i2
    case (Int32(i1), Int32(i2)) => i1 == i2
    case (Int64(i1), Int64(i2)) => i1 == i2
    case (Str(s1), Str(s2)) => s1 == s2
    case (Tag(_, ident1, e1, _, _), Tag(_, ident2, e2, _, _)) =>
      ident1.name == ident2.name && isEq(e1, e2)
    case (Tuple(elms1, _, _), Tuple(elms2, _, _)) =>
      (elms1 zip elms2) forall {
        case (e1, e2) => isEq(e1, e2)
      }
    case _ => throw InternalCompilerException(s"The arguments 'exp1' and 'exp2' must be values, but got: '$exp1' and '$exp2'.")
  }

  /**
    * Renames all free occurrence of the variable name `src` by the name `dst` in the given expression `exp`.
    *
    * NB: The variable name `dst` must not occur in the expression `exp` (i.e. it must be free!).
    */
  private def rename(src: Name.Ident, dst: Name.Ident, exp: Expression): Expression = exp match {
    case Unit => Unit
    case True => True
    case False => False
    case Int8(i) => Int8(i)
    case Int16(i) => Int16(i)
    case Int32(i) => Int32(i)
    case Int64(i) => Int64(i)
    case Str(s) => Str(s)
    case Var(ident, offset, tpe, loc) =>
      if (ident.name == src.name)
        Var(dst, offset, tpe, loc)
      else
        Var(ident, offset, tpe, loc)
    case Ref(name, tpe, loc) => Ref(name, tpe, loc)
    case Lambda(args, body, tpe, loc) =>
      assert(args.forall(_.ident.name != dst.name), s"The variable name ${dst.name} occurs in the given expression!")

      // Check if the variable `src` is bound by a formal argument.
      val bound = args.exists(_.ident.name == src.name)
      if (bound) {
        // Case 1: The variable `src` is bound by the lambda.
        // Nothing more to be done, so return the original lambda.
        Lambda(args, body, tpe, loc)
      } else {
        // Case 2: The variable `src` is *NOT* bound by the lambda.
        // Continue renaming in the body expression.
        Lambda(args, rename(src, dst, body), tpe, loc)
      }
    case Hook(hook, tpe, loc) => Hook(hook, tpe, loc)
    case Apply(lambda, args, tpe, loc) =>
      Apply(rename(src, dst, lambda), args.map(a => rename(src, dst, a)), tpe, loc)
    case Unary(op, e, tpe, loc) =>
      Unary(op, rename(src, dst, e), tpe, loc)
    case Binary(op, e1, e2, tpe, loc) =>
      Binary(op, rename(src, dst, e1), rename(src, dst, e2), tpe, loc)
    case IfThenElse(e1, e2, e3, tpe, loc) =>
      IfThenElse(rename(src, dst, e1), rename(src, dst, e2), rename(src, dst, e3), tpe, loc)
    case Let(ident, offset, e1, e2, tpe, loc) =>
      assert(ident.name != dst.name, s"The variable name ${dst.name} occurs in the given expression!")

      // Check if variable `src` is bound by the let-binding.
      val bound = ident.name == src.name
      if (bound) {
        // Case 1: The variable `src` is bound by the let-binding.
        // Only perform renaming inside the value expression, but not its body.
        Let(ident, offset, rename(src, dst, e1), e2, tpe, loc)
      } else {
        // Case 2: The variable `src` is *NOT* bound by the let-binding.
        // Perform renaming inside both the value and body expressions.
        Let(ident, offset, rename(src, dst, e1), rename(src, dst, e2), tpe, loc)
      }
    case Tag(enum, tag, e, tpe, loc) =>
      Tag(enum, tag, rename(src, dst, e), tpe, loc)
    case CheckTag(tag, e, loc) =>
      CheckTag(tag, rename(src, dst, e), loc)
    case GetTagValue(tag, e, tpe, loc) =>
      GetTagValue(tag, rename(src, dst, e), tpe, loc)
    case Tuple(elms, tpe, loc) =>
      Tuple(elms map (e => rename(src, dst, e)), tpe, loc)
    case GetTupleIndex(e, offset, tpe, loc) =>
      GetTupleIndex(rename(src, dst, e), offset, tpe, loc)
    case UserError(tpe, loc) => UserError(tpe, loc)
    case MatchError(tpe, loc) => MatchError(tpe, loc)
    case SwitchError(tpe, loc) => SwitchError(tpe, loc)
    case FSet(elms, tpe, loc) => throw InternalCompilerException("Unsupported.")
    case CheckNil(e, loc) => throw InternalCompilerException("Unsupported.")
    case CheckCons(e, loc) => throw InternalCompilerException("Unsupported.")
    case ApplyRef(name, args, tpe, loc) => throw InternalCompilerException("Deprecated feature.")
  }

  /**
    * Replaces all free (unbound) occurrences of the variable `ident`
    * with the expression `exp` in the expression `exp`.
    */
  private def substitute(src: Name.Ident, dst: Expression, exp: Expression)(implicit genSym: GenSym): Expression = {
    def visit(exp: Expression): Expression = exp match {
      case Unit => Unit
      case True => True
      case False => False
      case Int8(i) => Int8(i)
      case Int16(i) => Int16(i)
      case Int32(i) => Int32(i)
      case Int64(i) => Int64(i)
      case Str(s) => Str(s)
      case Var(ident, offset, tpe, loc) =>
        if (ident.name == src.name) {
          assert(tpe == dst.tpe, s"Type mismatch: '${tpe}' vs. '${dst.tpe}'.")
          dst
        }
        else
          Var(ident, offset, tpe, loc)
      case Ref(name, tpe, loc) => Ref(name, tpe, loc)
      case Lambda(args, body, tpe, loc) =>
        // Check if the variable `src` is bound by a formal argument.
        val bound = args.exists(a => a.ident.name == src.name)
        if (bound) {
          // Case 1: The variable `src` is bound by a formal argument.
          // Nothing more to be done, so return the original lambda.
          Lambda(args, body, tpe, loc)
        } else {
          // Case 2: The variable `src` is *NOT* bound by a formal argument.
          // Generate a fresh variable for the formal argument to avoid capture,
          // rename the argument and perform substitution in the body.

          // Introduce fresh variables for every formal argument to avoid capture.
          val freshVars = args.map(_ => genSym.fresh2())

          // Rename arguments.
          val args2 = (args zip freshVars) map {
            case (arg, freshVar) => arg.copy(ident = freshVar)
          }

          // Rename in body.
          val body2 = (args zip freshVars).foldLeft(body) {
            case (acc, (arg, freshVar)) =>
              val argVar = arg.ident
              rename(argVar, freshVar, acc)
          }

          Lambda(args2, visit(body2), tpe, loc)
        }
      case Hook(hook, tpe, loc) => Hook(hook, tpe, loc)
      case Apply(lambda, args, tpe, loc) =>
        Apply(visit(lambda), args.map(a => visit(a)), tpe, loc)
      case Unary(op, e, tpe, loc) =>
        Unary(op, visit(e), tpe, loc)
      case Binary(op, e1, e2, tpe, loc) =>
        Binary(op, visit(e1), visit(e2), tpe, loc)
      case IfThenElse(e1, e2, e3, tpe, loc) =>
        IfThenElse(visit(e1), visit(e2), visit(e3), tpe, loc)
      case Let(ident, offset, e1, e2, tpe, loc) =>
        // Check if the variable `src` is bound by the let-binding.
        val bound = ident.name == src.name
        if (bound) {
          // Case 1: The variable `src` is bound by the let.
          // Only perform substitution inside the value expression, but not the body.
          Let(ident, offset, visit(e1), e2, tpe, loc)
        } else {
          // Case 2: The variable `src` is *NOT* bound by the let.
          // Generate a fresh variable for the let-binding to avoid capture
          // and perform substitution in both the value and body expressions.
          val freshVar = genSym.fresh2()
          val bodyExp = rename(ident, freshVar, e2)
          Let(freshVar, offset, visit(e1), visit(bodyExp), tpe, loc)
        }
      case Tag(enum, tag, e, tpe, loc) =>
        Tag(enum, tag, visit(e), tpe, loc)
      case CheckTag(tag, e, loc) =>
        CheckTag(tag, visit(e), loc)
      case GetTagValue(tag, e, tpe, loc) =>
        GetTagValue(tag, visit(e), tpe, loc)
      case Tuple(elms, tpe, loc) =>
        Tuple(elms map (e => visit(e)), tpe, loc)
      case GetTupleIndex(e, offset, tpe, loc) =>
        GetTupleIndex(visit(e), offset, tpe, loc)
      case UserError(tpe, loc) => UserError(tpe, loc)
      case MatchError(tpe, loc) => MatchError(tpe, loc)
      case SwitchError(tpe, loc) => SwitchError(tpe, loc)
      case FSet(elms, tpe, loc) => throw InternalCompilerException("Unsupported.")
      case CheckNil(e, loc) => throw InternalCompilerException("Unsupported.")
      case CheckCons(e, loc) => throw InternalCompilerException("Unsupported.")
      case ApplyRef(name, args, tpe, loc) => throw InternalCompilerException("Deprecated feature.")
    }

    visit(exp)
  }

  /**
    * Returns `true` iff the given expression `e` is a value.
    */
  private def isValue(e: Expression): Boolean = e match {
    case Unit => true
    case True => true
    case False => true
    case Int8(i) => true
    case Int16(i) => true
    case Int32(i) => true
    case Int64(i) => true
    case Str(s) => true
    case Tag(enum, tag, e1, tpe, loc) => isValue(e1)
    case Tuple(elms, tpe, loc) => elms forall isValue
    case UserError(tpe, loc) => true
    case MatchError(tpe, loc) => true
    case SwitchError(tpe, loc) => true
    case _ => false
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
