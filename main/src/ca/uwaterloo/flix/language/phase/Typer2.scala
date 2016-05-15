package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.mutable

object Typer2 {

  /**
    * A common super-type for type constraints.
    */
  sealed trait TypeConstraint

  object TypeConstraint {

    /**
      * A type constraint that requires `t1` to unify with `t2`.
      */
    case class Eq(t1: Type, t2: Type) extends TypeConstraint

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
        def visitExp(e0: TypedAst.Expression, tenv: Map[Name.Ident, Type]): Type = e0 match {
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
            val tpe1 = visitExp(exp1, tenv)

            val r = Type.Var(genSym.fresh2().name) // TODO
            //(TypeConstraint.EqType(tpe1, Type.Lambda(tpes, r)) :: c1 ::: cs.flatten, r)
            r

          /*
           * Unary expression.
           */
          case TypedAst.Expression.Unary(op, exp1, _, _) => op match {
            case UnaryOperator.LogicalNot =>
              val tpe = visitExp(exp1, tenv)
              constraints += TypeConstraint.Eq(tpe, Type.Bool)
              Type.Bool

            case UnaryOperator.Plus | UnaryOperator.Minus =>
              val tpe = visitExp(exp1, tenv)
              constraints += TypeConstraint.OneOf(tpe, NumTypes)
              tpe

            case UnaryOperator.BitwiseNegate =>
              val tpe = visitExp(exp1, tenv)
              constraints += TypeConstraint.OneOf(tpe, IntTypes)
              tpe
          }

          /*
           * Binary expression.
           */
          // TODO: Check if we can use stuff like ArithmeticOperator, etc.
          case TypedAst.Expression.Binary(op, e1, e2, _, _) => op match {
            case BinaryOperator.Plus | BinaryOperator.Minus | BinaryOperator.Times | BinaryOperator.Divide =>
              val tpe1 = visitExp(e1, tenv)
              val tpe2 = visitExp(e2, tenv)
              constraints += TypeConstraint.Eq(tpe1, tpe2)
              constraints += TypeConstraint.OneOf(tpe1, NumTypes)
              constraints += TypeConstraint.OneOf(tpe2, NumTypes)
              tpe1

            case BinaryOperator.Modulo => ???

            case BinaryOperator.Exponentiate => ???

            case BinaryOperator.Equal | BinaryOperator.NotEqual => ???

            case BinaryOperator.Less | BinaryOperator.LessEqual | BinaryOperator.Greater | BinaryOperator.GreaterEqual =>
              val tpe1 = visitExp(e1, tenv)
              val tpe2 = visitExp(e2, tenv)
              // TODO
              constraints += TypeConstraint.Eq(tpe1, tpe2)

              Type.Bool

            // TODO: Logical Operator, can we shorten?
            case BinaryOperator.LogicalAnd | BinaryOperator.LogicalOr | BinaryOperator.Implication | BinaryOperator.Biconditional => ???

            // TODO: BitwiseOperator, can we shorten?
            case BinaryOperator.BitwiseAnd | BinaryOperator.BitwiseOr | BinaryOperator.BitwiseXor | BinaryOperator.BitwiseLeftShift | BinaryOperator.BitwiseRightShift => ???

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
            val tpe1 = visitExp(exp1, tenv)
            val tpe2 = visitExp(exp2, tenv)
            val tpe3 = visitExp(exp3, tenv)

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
          case TypedAst.Expression.Switch(rules, tpe, loc) => ???

          /*
           * Tag expression.
           */
          case TypedAst.Expression.Tag(name, ident, exp, tpe, loc) => ???

          /*
           * Tuple expression.
           */
          case TypedAst.Expression.Tuple(elms, tpe, loc) => ???

          /*
           * Set expression.
           */
          case TypedAst.Expression.Set(elms, tpe, loc) => ???

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
          case TypedAst.Expression.Existential(params, e, loc) => ???

          /*
           * Universal expression.
           */
          case TypedAst.Expression.Universal(params, e, loc) => ???

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
      case TypeConstraint.OneOf(tpe1, ts) => ???
      // try to unify each, and require at least one?
      // how to ensure unique typing? need list of maps?

    }

    def unify(tpe1: Type, tp2: Type, tenv: Map[String, Type]): Map[String, Type] = ??? // TODO: Probably needs to return Validation.

  }


}
