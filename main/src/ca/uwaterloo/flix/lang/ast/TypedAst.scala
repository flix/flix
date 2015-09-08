package ca.uwaterloo.flix.lang.ast

sealed trait TypedAst

object TypedAst {

  /**
   * A common super-type for typed AST nodes which represent expressions.
   */
  sealed trait Expression extends TypedAst

  /**
   * A typed AST node which represents a reference to a variable.
   */
  case class Var(name: String, tpe: Type) extends Expression

  /**
   * A typed AST node which represents unary expressions.
   */
  case class Unary(op: UnaryOperator, e: Expression, tpe: Type) extends Expression

  /**
   * A typed AST node which represents binary expressions.
   */
  case class Binary(e1: Expression, op: BinaryOperator, e2: Expression, tpe: Type) extends Expression


  /**
   * A typed AST node which represents an if-then-else expression.
   */
  case class IfThenElse(e1: Expression, e2: Expression, e3: Expression, tpe: Type) extends Expression


  sealed trait Type

}
