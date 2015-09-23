package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.Name

sealed trait Value {
  def toBool: Boolean = this.asInstanceOf[Value.Bool].b

  def toInt: Int = this.asInstanceOf[Value.Int].i

  def toStr: String = this.asInstanceOf[Value.Str].s
}

object Value {
  case object Unit extends Value

  case class Bool(b: scala.Boolean) extends Value

  case class Int(i: scala.Int) extends Value

  case class Str(s: java.lang.String) extends Value

  case class Tag(name: Name.Resolved, ident: String, value: Value) extends Value

  case class Tuple(elms: List[Value]) extends Value

  // ???
  //case class Lambda()
}