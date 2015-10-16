package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Name, TypedAst}

import scala.collection.mutable

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
  case object Unit extends Value

  /***************************************************************************
   * Value.Bool implementation                                               *
   ***************************************************************************/

  final class Bool private[Value] (val b: scala.Boolean) extends Value {
    override val toString: String = s"Value.Bool($b)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Bool => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = b.hashCode
  }

  object Bool {
    def unapply(v: Value.Bool): Option[scala.Boolean] = Some(v.b)
  }

  private val TRUE = new Value.Bool(true)
  private val FALSE = new Value.Bool(false)

  def mkBool(b: scala.Boolean) = if (b) TRUE else FALSE

  /***************************************************************************
   * Value.Int implementation                                                *
   ***************************************************************************/

  final class Int private[Value] (val i: scala.Int) extends Value {
    override val toString: String = s"Value.Int($i)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Int => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = i.hashCode
  }

  object Int {
    def unapply(v: Value.Int): Option[scala.Int] = Some(v.i)
  }

  // TODO: Need to use weak (or soft?) references so cache doesn't grow without bound
  private val intCache = mutable.HashMap[scala.Int, Value.Int]()

  def mkInt(i: scala.Int) = if (intCache.contains(i)) {
    intCache(i)
  } else {
    val ret = new Value.Int(i)
    intCache(i) = ret
    ret
  }



  case class Str(s: java.lang.String) extends Value

  case class Tag(name: Name.Resolved, ident: String, value: Value) extends Value

  case class Tuple(elms: List[Value]) extends Value

  case class Closure(formals: List[TypedAst.FormalArg], body: TypedAst.Expression, env: Interpreter.Env) extends Value

}