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
    case Value.Tag(enum, tag, value) => enum + "." + tag + "(" + value.pretty + ")"
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
    override val toString: java.lang.String = s"Value.Bool($b)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Bool => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = b.hashCode
  }

  object Bool {
    def unapply(v: Value.Bool): Option[scala.Boolean] = Some(v.b)
  }

  val True = new Value.Bool(true)
  val False = new Value.Bool(false)

  /***************************************************************************
   * Value.Int implementation                                                *
   ***************************************************************************/

  final class Int private[Value] (val i: scala.Int) extends Value {
    override val toString: java.lang.String = s"Value.Int($i)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Int => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = i.hashCode
  }

  object Int {
    def unapply(v: Value.Int): Option[scala.Int] = Some(v.i)
  }

  // TODO(mhyee): Need to use weak (or soft?) references so cache doesn't grow without bound
  private val intCache = mutable.HashMap[scala.Int, Value.Int]()

  def mkInt(i: scala.Int) = if (intCache.contains(i)) {
    intCache(i)
  } else {
    val ret = new Value.Int(i)
    intCache(i) = ret
    ret
  }

  /***************************************************************************
   * Value.Str implementation                                                *
   ***************************************************************************/

  final class Str private[Value] (val s: java.lang.String) extends Value {
    override val toString: java.lang.String = s"Value.Str($s)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Str => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = s.hashCode
  }

  object Str {
    def unapply(v: Value.Str): Option[java.lang.String] = Some(v.s)
  }

  // TODO(mhyee): Need to use weak (or soft?) references so cache doesn't grow without bound
  private val strCache = mutable.HashMap[java.lang.String, Value.Str]()

  def mkStr(s: java.lang.String) = if (strCache.contains(s)) {
    strCache(s)
  } else {
    val ret = new Value.Str(s)
    strCache(s) = ret
    ret
  }

  /***************************************************************************
   * Value.Tag implementation                                                *
   ***************************************************************************/

  final class Tag private[Value] (val enum: Name.Resolved, val tag: java.lang.String, val value: Value) extends Value {
    override val toString: java.lang.String = s"Value.Tag($enum, $tag, $value)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Tag => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = (enum, tag, value).hashCode
  }

  object Tag {
    def unapply(v: Value.Tag): Option[(Name.Resolved, java.lang.String, Value)] = Some((v.enum, v.tag, v.value))
  }

  // TODO(mhyee): Need to use weak (or soft?) references so cache doesn't grow without bound
  private val tagCache = mutable.HashMap[(Name.Resolved, java.lang.String, Value), Value.Tag]()

  def mkTag(e: Name.Resolved, t: java.lang.String, v: Value) = {
    val triple = (e, t, v)
    if (tagCache.contains(triple)) {
      tagCache(triple)
    } else {
      val ret = new Value.Tag(e, t, v)
      tagCache(triple) = ret
      ret
    }
  }

  /***************************************************************************
   * Value.Tuple, Value.Closure implementations                              *
   ***************************************************************************/

  case class Tuple(elms: List[Value]) extends Value

  case class Closure(formals: List[TypedAst.FormalArg], body: TypedAst.Expression, env: Interpreter.Env) extends Value
}
