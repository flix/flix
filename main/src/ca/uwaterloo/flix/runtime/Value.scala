package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.{Name, TypedAst}

sealed trait Value {
  def toBool: Boolean = {
    assert(this.isInstanceOf[Value.Bool], "Expected a Value.Bool.")
    this.asInstanceOf[Value.Bool].b
  }

  def toInt: Int = {
    assert(this.isInstanceOf[Value.Int], "Expected a Value.Int.")
    this.asInstanceOf[Value.Int].i
  }

  def toStr: String = {
    assert(this.isInstanceOf[Value.Str], "Expected a Value.Str.")
    this.asInstanceOf[Value.Str].s
  }
}

object Value {
  case object Unit extends Value

  case class Bool(b: scala.Boolean) extends Value

  case class Int(i: scala.Int) extends Value

  case class Str(s: java.lang.String) extends Value

  case class Tag(name: Name.Resolved, ident: String, value: Value) extends Value

  case class Tuple(elms: List[Value]) extends Value

  case class Closure(formals: List[TypedAst.FormalArg], body: TypedAst.Expression, env: Interpreter.Env) extends Value
}