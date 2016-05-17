package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.mutable

object Typer2 {

  // TODO: Switch to weededast or namedast etc.
  // Need unique ids for each expr.

  /**
    * A common super-type for type constraints.
    */
  sealed trait TypeConstraint

  object TypeConstraint {

    /**
      * A type constraint that requires `t1` to unify with `t2`.
      */
    case class Eq(t1: Type, t2: Type) extends TypeConstraint

    case class AllEq(tpes: List[Type]) extends TypeConstraint

    /**
      * A type constraint that requires `t1` to unify with one of the types `ts`.
      */
    case class OneOf(t1: Type, ts: Set[Type]) extends TypeConstraint

  }

  /**
    * The integral types.
    */
  val IntTypes: Set[Type] = Set(
    Type.Int8,
    Type.Int16,
    Type.Int32,
    Type.Int64
  )

  /**
    * The number types (i.e. integral and fractional types).
    */
  val NumTypes: Set[Type] = Set(
    Type.Float32,
    Type.Float64,
    Type.Int8,
    Type.Int16,
    Type.Int32,
    Type.Int64
  )

  /**
    * Phase 1: Constraint Generation.
    */
  object ConstraintGeneration {

    object Expressions {

      /**
        * Generates type constraints for the given expression `exp0`.
        */
      def gen(exp0: TypedAst.Expression)(implicit genSym: GenSym): Unit = {

        /*
         * A mutable set to hold the collected constraints.
         */
        val constraints = mutable.Set.empty[TypeConstraint]

        /**
          * Generates type constraints for the given expression `e0` under the given type environment `tenv`.
          */
        def visitExp(e0: TypedAst.Expression, tenv0: Map[Name.Ident, Type]): Type = e0 match {
          /*
           * Literal expression.
           */
          case TypedAst.Expression.Lit(lit, _, _) => lit match {
            case TypedAst.Literal.Unit(_) => Type.Unit
            case TypedAst.Literal.Bool(_, _) => Type.Bool
            case TypedAst.Literal.Char(_, _) => Type.Char
            case TypedAst.Literal.Float32(_, _) => Type.Float32
            case TypedAst.Literal.Float64(_, _) => Type.Float64
            case TypedAst.Literal.Int8(_, _) => Type.Int8
            case TypedAst.Literal.Int16(_, _) => Type.Int16
            case TypedAst.Literal.Int32(_, _) => Type.Int32
            case TypedAst.Literal.Int64(_, _) => Type.Int64
            case TypedAst.Literal.BigInt(_, _) => Type.BigInt
            case TypedAst.Literal.Str(_, _) => Type.Str
          }

          /*
           * Variable expression.
           */
          case TypedAst.Expression.Var(ident, _, _) =>
            // TODO: Lookup the type in the type environment.
            ???

          /*
           * Ref expression.
           */
          case TypedAst.Expression.Ref(name, tpe, loc) => ???

          /*
           * Hook expression.
           */
          case TypedAst.Expression.Hook(hook, tpe, loc) => ???

          /*
           * Lambda expression.
           */
          case TypedAst.Expression.Lambda(args, body, tpe, loc) => ???

          /*
           * Apply expression.
           */
          case TypedAst.Expression.Apply(exp1, args, _, _) =>
            // TODO
            val tpe1 = visitExp(exp1, tenv0)

            val r = Type.Var(genSym.fresh2().name) // TODO
            //(TypeConstraint.EqType(tpe1, Type.Lambda(tpes, r)) :: c1 ::: cs.flatten, r)
            r

          /*
           * Unary expression.
           */
          case TypedAst.Expression.Unary(op, exp1, _, _) => op match {
            case UnaryOperator.LogicalNot =>
              val tpe = visitExp(exp1, tenv0)
              constraints += TypeConstraint.Eq(tpe, Type.Bool)
              Type.Bool

            case UnaryOperator.Plus | UnaryOperator.Minus =>
              val tpe = visitExp(exp1, tenv0)
              constraints += TypeConstraint.OneOf(tpe, NumTypes)
              tpe

            case UnaryOperator.BitwiseNegate =>
              val tpe = visitExp(exp1, tenv0)
              constraints += TypeConstraint.OneOf(tpe, IntTypes)
              tpe
          }

          /*
           * Binary expression.
           */
          // TODO: Check if we can use stuff like ArithmeticOperator, etc.
          case TypedAst.Expression.Binary(op, e1, e2, _, _) => op match {
            case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              constraints += TypeConstraint.OneOf(tpe1, NumTypes)
              constraints += TypeConstraint.OneOf(tpe2, NumTypes)
              tpe1

            case BinaryOperator.Modulo =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              constraints += TypeConstraint.OneOf(tpe1, NumTypes)
              constraints += TypeConstraint.OneOf(tpe2, NumTypes)
              tpe1

            case BinaryOperator.Exponentiate =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              constraints += TypeConstraint.OneOf(tpe1, NumTypes)
              constraints += TypeConstraint.OneOf(tpe2, NumTypes)
              tpe1

            case BinaryOperator.Equal | BinaryOperator.NotEqual =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              Type.Bool

            case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              Type.Bool

            // TODO: Logical Operator, can we shorten?
            case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr | BinaryOperator.Implication | BinaryOperator.Biconditional =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, Type.Bool)
              constraints += TypeConstraint.Eq(tpe2, Type.Bool)
              Type.Bool

            // TODO: BitwiseOperator, can we shorten?
            case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor | BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift =>
              val tpe1 = visitExp(e1, tenv0)
              val tpe2 = visitExp(e2, tenv0)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              constraints += TypeConstraint.OneOf(tpe1, NumTypes)
              constraints += TypeConstraint.OneOf(tpe2, NumTypes)
              tpe1

          }

          /*
           * Let expression.
           */
          case TypedAst.Expression.Let(ident, exp1, exp2, tpe, _) =>
            // update type environment and recurse
            // TODO
            ???

          /*
           * If-then-else expression.
           */
          case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
            val tpe1 = visitExp(exp1, tenv0)
            val tpe2 = visitExp(exp2, tenv0)
            val tpe3 = visitExp(exp3, tenv0)

            constraints += TypeConstraint.Eq(tpe1, Type.Bool)
            constraints += TypeConstraint.Eq(tpe2, tpe3)
            tpe2

          /*
           * Match expression.
           */
          case TypedAst.Expression.Match(exp1, rules, tpe, loc) => ???

          /*
           * Switch expression.
           */
          case TypedAst.Expression.Switch(rules, tpe, loc) =>
            val bodyTypes = mutable.ListBuffer.empty[Type]
            for ((cond, body) <- rules) {
              val condType = visitExp(cond, tenv0)
              bodyTypes += visitExp(body, tenv0)
              constraints += TypeConstraint.Eq(condType, Type.Bool)
            }

            constraints += TypeConstraint.AllEq(bodyTypes.toList)

            bodyTypes.head // TODO: Or generate fresh symbol?

          /*
           * Tag expression.
           */
          case TypedAst.Expression.Tag(enum, tag, exp, tpe, loc) =>
            // TODO
            val tpe = visitExp(exp, tenv0)
            Type.Enum(enum, ???)

          /*
           * Tuple expression.
           */
          case TypedAst.Expression.Tuple(elms, tpe, loc) =>
            val tpes = elms.map(e => visitExp(e, tenv0))
            Type.Tuple(tpes)

          /*
           * Set expression.
           */
          case TypedAst.Expression.Set(elms, tpe, loc) =>
            val tpes = elms.map(e => visitExp(e, tenv0))
            constraints += TypeConstraint.AllEq(tpes)
            Type.FSet(tpes.head)

          // FNone
          // FSome
          // FNil
          // FList
          // FVec
          // FSet
          // FMap
          // GetIndex
          // PutIndex

          /*
           * Existential expression.
           */
          // TODO: Weeder should check that arguments are not duplicated
          case TypedAst.Expression.Existential(params, e, loc) =>
            val tenv = params.foldLeft(tenv0) {
              case (macc, Ast.FormalParam(id, t)) => macc + (id -> t)
            }
            val tpe = visitExp(e, tenv)
            constraints += TypeConstraint.Eq(tpe, Type.Bool)
            Type.Bool

          /*
           * Universal expression.
           */
          // TODO: Weeder should check that arguments are not duplicated
          case TypedAst.Expression.Universal(params, e, loc) =>
            val tenv = params.foldLeft(tenv0) {
              case (macc, Ast.FormalParam(id, t)) => macc + (id -> t)
            }
            val tpe = visitExp(e, tenv)
            constraints += TypeConstraint.Eq(tpe, Type.Bool)
            Type.Bool

          /*
           * Ascribe expression.
           */
          // TODO

          /*
           * User Error expression.
           */
          case TypedAst.Expression.Error(tpe, loc) =>
            // TODO
            ???

        }

        /**
          * Generates type constraints for the given pattern `p0` under the given type environment `tenv`.
          */
        def visitPat(p0: TypedAst.Pattern, tenv: Map[Name.Ident, Type]): Type = p0 match {
          case TypedAst.Pattern.Wildcard(tpe, loc) => tpe
          case TypedAst.Pattern.Var(ident, tpe, loc) => tpe
          case TypedAst.Pattern.Lit(lit, tpe, _) => lit match {
            case TypedAst.Literal.Unit(loc) => Type.Unit
            case TypedAst.Literal.Bool(_, _) => Type.Bool
            case TypedAst.Literal.Char(_, _) => Type.Char
            case TypedAst.Literal.Float32(_, _) => Type.Float32
            case TypedAst.Literal.Float64(_, _) => Type.Float64
            case TypedAst.Literal.Int8(_, _) => Type.Int8
            case TypedAst.Literal.Int16(_, _) => Type.Int16
            case TypedAst.Literal.Int32(_, _) => Type.Int32
            case TypedAst.Literal.Int64(_, _) => Type.Int64
            case TypedAst.Literal.BigInt(_, _) => Type.BigInt
            case TypedAst.Literal.Str(_, _) => Type.Str
          }

          case TypedAst.Pattern.Tag(enum, tag, p1, tpe, loc) => ???

          case TypedAst.Pattern.Tuple(elms, tpe, loc) =>
            val tpes = elms.map(e => visitPat(e, tenv))
            Type.Tuple(tpes)
        }

        visitExp(exp0, Map.empty)

      }
    }

  }

  /**
    * Phase 2: Unification.
    */
  object Unification {

    // TODO: Possibly return map from names to types.
    def unify(cs: List[TypeConstraint.Eq]): List[TypeConstraint.Eq] = ???

    def unify(tc: TypeConstraint, tenv: Map[String, Type]): Map[String, Type] = tc match {
      // TODO: Probably needs to return Validation.
      case TypeConstraint.Eq(tpe1, tpe2) => unify(tpe1, tpe2, tenv)
      case TypeConstraint.AllEq(tpes) => ??? // TODO
      case TypeConstraint.OneOf(tpe1, ts) => ???
      // try to unify each, and require at least one?
      // how to ensure unique typing? need list of maps?

    }

    def unify(tpe1: Type, tp2: Type, tenv: Map[String, Type]): Map[String, Type] = ??? // TODO: Probably needs to return Validation.

  }


}
