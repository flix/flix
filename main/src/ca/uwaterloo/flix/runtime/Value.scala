package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.Name

//TODO(mhyee): Do we have standard way for throwing errors?

sealed trait Value {
  def toBool: Boolean = this match {
    case Value.Bool(b) => b
    case _ => ???
  }

  def toInt: Int = this match {
    case Value.Int(i) => i
    case _ => ???
  }

  def toStr: String = this match {
    case Value.Str(s) => s
    case _ => ???
  }
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