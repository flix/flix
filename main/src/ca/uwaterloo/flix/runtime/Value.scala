package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst.Type

import java.lang.reflect.Method
import scala.collection.mutable

sealed trait Value {
  def toBool: Boolean = this.asInstanceOf[Value.Bool].b

  def toInt: Int = this.asInstanceOf[Value.Int].i

  def toStr: String = this.asInstanceOf[Value.Str].s

  def toSet: Set[Value] = this.asInstanceOf[Value.Set].elms

  def toJava: java.lang.Object = (this: @unchecked) match {
    case Value.Bool(b) => boolean2Boolean(b)
    case Value.Int(i) => int2Integer(i)
    case Value.Str(s) => s
    case Value.Tuple(elms) if (2 to 5).contains(elms.size) =>
      val javaElms = elms.map(_.toJava)
      javaElms.size match {
        case 2 => (javaElms(0), javaElms(1))
        case 3 => (javaElms(0), javaElms(1), javaElms(2))
        case 4 => (javaElms(0), javaElms(1), javaElms(2), javaElms(3))
        case 5 => (javaElms(0), javaElms(1), javaElms(2), javaElms(3), javaElms(4))
      }
    case Value.Set(elms) => elms.map(_.toJava)
    case Value.Native(v) => v
    case Value.Unit | Value.Tag(_) | Value.Tuple(_) | Value.Closure(_, _, _) | Value.NativeMethod(_) => this
  }

  //  TODO: Figure out a place to put all the formatting functions.
  def pretty: String = this match {
    case Value.Unit => "()"
    case Value.Bool(b) => b.toString
    case Value.Int(i) => i.toString
    case Value.Str(s) => s.toString
    case Value.Tag(enum, tag, value) => s"$enum.$tag(${value.pretty})"
    case Value.Tuple(elms) => "(" + elms.map(_.pretty).mkString(",") + ")"
    case Value.Set(elms) => "{" + elms.map(_.pretty).mkString(",") + "}"
    case Value.Closure(_, _, _) => ???
    case Value.Native(v) => s"Native($v)"
    case Value.NativeMethod(m) => ???
  }
}

object Value {

  case object Unit extends Value

  /** *************************************************************************
    * Value.Bool implementation                                               *
    * **************************************************************************/

  final class Bool private[Value](val b: scala.Boolean) extends Value {
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

  /** *************************************************************************
    * Value.Tag implementation                                                *
    * **************************************************************************/

  final class Tag private[Value](val enum: Name.Resolved, val tag: java.lang.String, val value: Value) extends Value {
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

  /** *************************************************************************
    * Value.Tuple, Value.Set, Value.Closure implementations                   *
    * **************************************************************************/

  case class Tuple(elms: List[Value]) extends Value

  case class Set(elms: scala.collection.immutable.Set[Value]) extends Value

  case class Closure(formals: List[TypedAst.FormalArg], body: TypedAst.Expression, env: Interpreter.Env) extends Value

  /** *************************************************************************
    * Value.Native, Value.NativeMethod implementations                        *
    * **************************************************************************/

  case class Native(value: AnyRef) extends Value

  case class NativeMethod(method: Method) extends Value

  /** *************************************************************************
    * Convert from native values to Flix values                               *
    * **************************************************************************/

  def java2flix(obj: AnyRef, tpe: Type): Value = tpe match {
    case Type.Bool => if (obj.asInstanceOf[java.lang.Boolean].booleanValue) Value.True else Value.False
    case Type.Int => Value.mkInt(obj.asInstanceOf[java.lang.Integer].intValue)
    case Type.Str => Value.mkStr(obj.asInstanceOf[java.lang.String])
    case Type.Tuple(typs) if (2 to 5).contains(typs.size) =>
      def makeTuple(elms: java.lang.Object*): Value.Tuple =
        Value.Tuple(elms.toList.zip(typs).map { case (e, t) => java2flix(e, t) })
      typs.size match {
        case 2 =>
          val t = obj.asInstanceOf[(java.lang.Object, java.lang.Object)]
          makeTuple(t._1, t._2)
        case 3 =>
          val t = obj.asInstanceOf[(java.lang.Object, java.lang.Object, java.lang.Object)]
          makeTuple(t._1, t._2, t._3)
        case 4 =>
          val t = obj.asInstanceOf[(java.lang.Object, java.lang.Object, java.lang.Object, java.lang.Object)]
          makeTuple(t._1, t._2, t._3, t._4)
        case 5 =>
          val t = obj.asInstanceOf[(java.lang.Object, java.lang.Object, java.lang.Object, java.lang.Object, java.lang.Object)]
          makeTuple(t._1, t._2, t._3, t._4, t._5)
      }
    case Type.Set(typ) =>
      Value.Set(obj.asInstanceOf[scala.collection.immutable.Set[java.lang.Object]].map(e => java2flix(e, typ)))
    case Type.Var(_) | Type.Unit | Type.Tag(_, _, _) | Type.Enum(_) | Type.Tuple(_) | Type.Set(_) | Type.Lambda(_, _) |
         Type.Predicate(_) | Type.Native(_) =>
      Value.Native(obj)
  }
}
