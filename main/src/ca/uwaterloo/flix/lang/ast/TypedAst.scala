package ca.uwaterloo.flix.lang.ast

sealed trait TypedAst

object TypedAst {

  // TODO

  sealed trait Expression extends TypedAst {
    def tpe: Type
  }

  object Expression {
    case class IfThenElse(e1: TypedAst.Expression, e2: TypedAst.Expression, e3: TypedAst.Expression, tpe: TypedAst.Type) extends TypedAst.Expression
  }


  sealed trait Type extends TypedAst

  object Type {

    case object Bool extends TypedAst.Type

  }


}
