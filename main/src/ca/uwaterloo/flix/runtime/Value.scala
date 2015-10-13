package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Name, TypedAst}

sealed trait Value {
  def toBool: Boolean = {
    this.asInstanceOf[Value.Bool].b
  }

  def toInt: Int = {
    this.asInstanceOf[Value.Int].i
  }

  def toStr: String = {
    this.asInstanceOf[Value.Str].s
  }

  //  TODO: Figure out a place to put all the formatting functions.
  def pretty: String = this match {
    case Value.Unit => "()"
    case Value.Bool(b) => b.toString
    case Value.Int(i) => i.toString
    case Value.Str(s) => s.toString
    case Value.Tag(enum, tag, value) => enum + "." + tag + value.pretty
    case Value.Tuple(elms) => "(" + (elms map (e => e.pretty)) + ")"
    case Value.Closure(_, _, _) => ???
  }
}

object Value {
  private val TRUE = new Bool(true)
  private val FALSE = new Bool(false)

  def mkBool(b: Boolean) = if (b) TRUE else FALSE

  case object Unit extends Value

  final class Bool private[Value] (val b: scala.Boolean) extends Value {
    override val toString: String = s"Value.Bool($b)"

    override def equals(other: Any): Boolean = other match {
      case that: Bool => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = b.hashCode
  }

  object Bool {
    def unapply(b: Bool): Option[scala.Boolean] = Some(b.b)
  }

  case class Int(i: scala.Int) extends Value

  case class Str(s: java.lang.String) extends Value

  case class Tag(name: Name.Resolved, ident: String, value: Value) extends Value

  case class Tuple(elms: List[Value]) extends Value

  case class Closure(formals: List[TypedAst.FormalArg], body: TypedAst.Expression, env: Interpreter.Env) extends Value

}