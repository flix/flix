package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast.{TypedAst, ResolvedAst}

import util.Validation
import util.Validation._

object Typer {
  // TODO

  import TypeError._

  sealed trait TypeError

  object TypeError {

  }


  def typecheck(rast: ResolvedAst) = ???

  def typeExpression(rast: ResolvedAst.Expression): Validation[TypedAst.Expression, TypeError] = rast match {
    case ResolvedAst.Expression.IfThenElse(re1, re2, re3) =>
      @@(typeExpression(re1), typeExpression(re2), typeExpression(re3)) flatMap {
        case (e1, e2, e3) =>
          val conditionType = eq(e1.tpe, TypedAst.Type.Bool)
          val expressionType = eq(e2.tpe, e3.tpe)
          #@(conditionType, expressionType) map {
            case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe)
          }
      }
  }

  def eq(tpe1: TypedAst.Type, tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

}
