package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.TypedAst

/**
  * Pretty printing of expressions.
  */
object PrettyExpression {

  /**
    * TypedAst.
    */
  def pretty(e0: TypedAst.Expression): String = e0 match {


    case _ => e0.toString
  }

}
