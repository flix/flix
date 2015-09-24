package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.{Name, TypedAst}

sealed trait Value {
  def toBool: Boolean = this match {
    case Value.Bool(b) => b
    case _ => throw new RuntimeException("Expected a Value.Bool.")
  }

  def toInt: Int = this match {
    case Value.Int(i) => i
    case _ => throw new RuntimeException("Expected a Value.Int.")
  }

  def toStr: String = this match {
    case Value.Str(s) => s
    case _ => throw new RuntimeException("Expected a Value.Str.")
  }
}

object Value {
  case object Unit extends Value

  case class Bool(b: scala.Boolean) extends Value

  case class Int(i: scala.Int) extends Value

  case class Str(s: java.lang.String) extends Value

  case class Tag(name: Name.Resolved, ident: String, value: Value) extends Value

  case class Tuple(elms: List[Value]) extends Value

  case class Closure(func: TypedAst.Expression, env: Interpreter.Env) extends Value
}