package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

import scala.collection.mutable

object Typer2 {

  sealed trait TypeConstraint

  object TypeConstraint {

    case class Eq(t1: Type, t2: Type) extends TypeConstraint

    case class OneOf(t1: Type, t2: Set[Type]) extends TypeConstraint

  }

  val IntTypes: Set[Type] = Set(
    Type.Int8,
    Type.Int16,
    Type.Int32,
    Type.Int64
  )

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
        def visit(e0: TypedAst.Expression, tenv: Map[Name.Ident, Type]): Type = e0 match {
          /*
           * Literal Expression.
           */
          case TypedAst.Expression.Lit(lit, _, _) => lit match {
            case TypedAst.Literal.Unit(loc) => Type.Unit
            case TypedAst.Literal.Bool(_, _) => Type.Bool
            case TypedAst.Literal.Char(_, _) => Type.Char
            case TypedAst.Literal.Float32(_, _) => Type.Float32
            case TypedAst.Literal.Float64(_, _) => Type.Float64
          }

          case TypedAst.Expression.Var(ident, _, _) =>
            ??? // TODO Look up in type environment.

          case TypedAst.Expression.Let(ident, exp1, exp2, tpe, _) =>
            // update type environment and recurse
            ???


          /*
           * Unary Expression.
           */
          case TypedAst.Expression.Unary(op, exp1, _, _) => op match {
            case UnaryOperator.LogicalNot =>
              val tpe = visit(exp1, tenv)
              constraints += TypeConstraint.Eq(tpe, Type.Bool)
              Type.Bool

            case UnaryOperator.Plus | UnaryOperator.Minus =>
              val tpe = visit(exp1, tenv)
              constraints += TypeConstraint.OneOf(tpe, NumTypes)
              tpe

            case UnaryOperator.BitwiseNegate =>
              val tpe = visit(exp1, tenv)
              constraints += TypeConstraint.OneOf(tpe, IntTypes)
              tpe
          }

          /*
           * Binary Expression.
           */
          case TypedAst.Expression.Binary(op, e1, e2, _, _) => op match {
            case BinaryOperator.Plus =>
              // TODO: Use EqTypeOneOf.
              ???

            case _: ComparisonOperator =>
              val tpe1 = visit(e1, tenv)
              val tpe2 = visit(e2, tenv)

              constraints += TypeConstraint.Eq(tpe1, tpe2)

              Type.Bool
          }

          case TypedAst.Expression.Apply(exp1, args, _, _) =>
            val tpe1 = visit(exp1, tenv)

            val r = Type.Var(genSym.fresh2().name) // TODO
            //(TypeConstraint.EqType(tpe1, Type.Lambda(tpes, r)) :: c1 ::: cs.flatten, r)
            r

          /*
           * If-then-else Expression.
           */
          case TypedAst.Expression.IfThenElse(exp1, exp2, exp3, _, loc) =>
            val tpe1 = visit(exp1, tenv)
            val tpe2 = visit(exp2, tenv)
            val tpe3 = visit(exp3, tenv)

            constraints += TypeConstraint.Eq(tpe1, Type.Bool)
            constraints += TypeConstraint.Eq(tpe2, tpe3)
            tpe2
        }


        visit(exp0, Map.empty)

      }
    }

  }

  /**
    * Phase 2: Unification.
    */
  object Unification {

    // TODO: Possibly return map from names to types.
    def unify(cs: List[TypeConstraint.Eq]): List[TypeConstraint.Eq] = ???

  }


}
