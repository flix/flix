package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.ast.TypedAst

object Typer2 {

  sealed trait TypeConstraint

  object TypeConstraint {

  }

  object ConstraintGeneration {


    object Expressions {

      /**
        * Generates type constraints for the given expression `exp0`.
        */
      def gen(exp0: TypedAst.Expression): TypeConstraint = exp0 match {
        case TypedAst.Expression.IfThenElse(e1, e2, e3, _, loc) =>
          ???
      }

    }

  }


  object Unification {

  }


}
