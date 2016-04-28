package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast._

object Typer2 {

  case class EqType(t1: Type, t2: Type)

  case class EqTypeOneOf(t1: Type, t2: Set[Type])

  object ConstraintGeneration {


    object Expressions {

      // TODO: Use mutable list of type constraints.

      /**
        * Generates type constraints for the given expression `exp0`.
        */
      def gen(exp0: TypedAst.Expression, tenv: Map[Name.Ident, Type])(implicit genSym: GenSym): (List[EqType], Type) = exp0 match {

        case TypedAst.Expression.Lit(TypedAst.Literal.Int32(_, _), _, _) => (Nil, Type.Int32)

        case TypedAst.Expression.Var(ident, _, _) =>
          ??? // TODO Look up in type environment.

        case TypedAst.Expression.Let(ident, exp1, exp2, tpe, _) =>
          // update type environment and recurse
          ???

        case TypedAst.Expression.Binary(op, e1, e2, _, _) => op match {
          case BinaryOperator.Plus =>
            // TODO: Use EqTypeOneOf.
            ???

          case _: ComparisonOperator =>
            val (c1, tpe1) = gen(e1, tenv)
            val (c2, tpe2) = gen(e2, tenv)

            (EqType(tpe1, tpe2) :: c1 ++ c2, Type.Bool)
        }

        case TypedAst.Expression.Apply(exp1, args, _, _) =>
          val (c1, tpe1) = gen(exp1, tenv)
          val (cs, tpes) = args.map {
            case e => gen(e, tenv)
          }.unzip

          val r = Type.Var(genSym.fresh2().name) // TODO
          (EqType(tpe1, Type.Lambda(tpes, r)) :: c1 ::: cs.flatten, r)

        case TypedAst.Expression.IfThenElse(e1, e2, e3, _, loc) =>
          val (c1, tpe1) = gen(e1, tenv)
          val (c2, tpe2) = gen(e2, tenv)
          val (c3, tpe3) = gen(e3, tenv)
          (EqType(tpe1, Type.Bool) :: EqType(tpe2, tpe3) :: c1 ++ c2 ++ c3, tpe2)
      }

    }

  }


  object Unification {

    // TODO: Possibly return map from names to types.
    def unify(cs: List[EqType]): List[EqType] = ???

  }


}
