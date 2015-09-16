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

  // TODO: locals: Map[String, TypedAst.Type], globals: Map[???, TypedAst.Type]
  def typeExpression(rast: ResolvedAst.Expression): Validation[TypedAst.Expression, TypeError] = rast match {
    case ResolvedAst.Expression.LocalVar(name, declaration, location) => ???

    case ResolvedAst.Expression.IfThenElse(re1, re2, re3) =>
      @@(typeExpression(re1), typeExpression(re2), typeExpression(re3)) flatMap {
        case (e1, e2, e3) =>
          val conditionType = expect(TypedAst.Type.Bool)(e1.tpe)
          val expressionType = expectEqual(e2.tpe, e3.tpe)
          #@(conditionType, expressionType) map {
            case tpe => TypedAst.Expression.IfThenElse(e1, e2, e3, tpe)
          }
      }
  }

  def expect(tpe1: TypedAst.Type)(tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

  def expectEqual(tpe1: TypedAst.Type, tpe2: TypedAst.Type): Validation[TypedAst.Type, TypeError] = ???

}
