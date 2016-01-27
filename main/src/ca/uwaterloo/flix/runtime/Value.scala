package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import scala.collection.mutable

import java.util

@deprecated("The value trait will be removed in the future. Please do not use it. " +
  "The replacement is to either use AnyRef or better, to use a generic parameter.", "0.1.0")
sealed trait Value

object Value {

  def pretty(o: AnyRef): String = o match {
    case Value.Unit => "()"
    case v: Value.Bool => v.b.toString
    case v: Value.Int => v.i.toString
    case v: Value.Str => v.s.toString
    case v: Value.Tag => s"${v.enum}.${v.tag}(${pretty(v.value)})"
    case Value.Tuple(elms) => "(" + elms.map(pretty).mkString(",") + ")"
    case Value.Set(elms) => "{" + elms.map(pretty).mkString(",") + "}"
    case Value.Closure(_, _, _) => ???
    case Value.Native(v) => s"Native($v)"
    case _ => o.toString
  }

  /////////////////////////////////////////////////////////////////////////////
  // Unit                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The Unit value.
    */
  case object Unit extends Value

  /////////////////////////////////////////////////////////////////////////////
  // Bool                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  @deprecated
  final class Bool private[Value](val b: scala.Boolean) extends Value {
    override val toString: java.lang.String = s"Value.Bool($b)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Bool => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = b.hashCode
  }

  /**
    * The true value.
    */
  @inline
  val True = new Value.Bool(true)

  /**
    * The false value.
    */
  @inline
  val False = new Value.Bool(false)

  /**
    * Constructs a bool from the given boolean `b`.
    */
  @inline
  def mkBool(b: Boolean): Bool = if (b) True else False

  /**
    * Casts the given reference `ref` to a primitive boolean.
    */
  @inline
  def cast2bool(ref: Any): Boolean = ref match {
    case Value.True => true
    case Value.False => false
    case o: java.lang.Boolean => o.booleanValue()
    case _ => throw new IllegalArgumentException()
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ints                                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Casts the given reference `ref` to an int32.
    */
  def cast2int32(ref: AnyRef): scala.Int = ref match {
    case v: Value.Int => v.i
    case o: java.lang.Integer => o.intValue()
    case _ => throw new IllegalArgumentException()
  }

  /** *************************************************************************
    * Value.Int implementation                                                *
    * **************************************************************************/

  final class Int private[Value](val i: scala.Int) extends Value {
    override val toString: java.lang.String = s"Value.Int($i)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Int => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = i.hashCode
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

  /** *************************************************************************
    * Value.Str implementation                                                *
    * **************************************************************************/

  final class Str private[Value](val s: java.lang.String) extends Value {
    override val toString: java.lang.String = s"Value.Str($s)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Str => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = s.hashCode
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

  /** *************************************************************************
    * Value.Tag implementation                                                *
    * **************************************************************************/

  final class Tag private[Value](val enum: Name.Resolved, val tag: java.lang.String, val value: Value) extends Value {
    override val toString: java.lang.String = s"Value.Tag($enum, $tag, $value)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Tag => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = 41 * (41 * (41 + enum.hashCode) + tag.hashCode) + value.hashCode
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

  /** *************************************************************************
    * Value.Tuple, Value.Set, Value.Closure implementations                   *
    * **************************************************************************/

  final case class Tuple(elms: Array[Value]) extends Value {
    override def toString: java.lang.String = s"Value.Tuple(Array(${elms.mkString(",")}))"

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Value.Tuple =>
        util.Arrays.equals(this.elms.asInstanceOf[Array[AnyRef]], that.elms.asInstanceOf[Array[AnyRef]])
      case _ => false
    }

    override def hashCode: scala.Int = util.Arrays.hashCode(elms.asInstanceOf[Array[AnyRef]])
  }

  case class Set(elms: scala.collection.immutable.Set[Value]) extends Value

  final case class Closure(formals: Array[String], body: TypedAst.Expression, env: mutable.Map[String, Value]) extends Value {
    override def toString: java.lang.String = s"Value.Closure(Array(${formals.mkString(",")}), $body, $env)"

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Value.Closure =>
        util.Arrays.equals(this.formals.asInstanceOf[Array[AnyRef]], that.formals.asInstanceOf[Array[AnyRef]]) &&
          this.body == that.body && this.env == that.env
      case _ => false
    }

    override def hashCode: scala.Int =
      41 * (41 * (41 + util.Arrays.hashCode(formals.asInstanceOf[Array[AnyRef]])) + body.hashCode) + env.hashCode
  }

  /** *************************************************************************
    * Value.Native, Value.HookClosure implementations                         *
    * **************************************************************************/

  final case class Native(value: AnyRef) extends Value

  final case class HookClosure(inv: Invokable) extends Value

  def mkList(list: List[Value]): Value = ??? // TODO

  def mkMap(map: Map[Value, Value]): Value = ??? // TODO

  def mkNone: Value = ??? // TODO

  def mkSome(v: Value): Value = ??? // TODO

}
