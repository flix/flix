package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.runtime.Interpreter.InternalRuntimeError
import scala.collection.immutable
import scala.collection.mutable

import java.util

object Value {


  def cast2str(ref: AnyRef): String = ref match {
    case v: Value.Str => v.s
    case o: java.lang.String => o
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def cast2tuple(ref: AnyRef): Array[AnyRef] = ref match {
    // case v: Value.Tuple => v.elms.map(e => new WrappedValue(e))
    //case o: Array[AnyRef] => o.map(e => new WrappedValue(e))
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def cast2tag(ref: AnyRef): Value.Tag = ref match {
    case v: Value.Tag => v
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  // TODO: return type
  def cast2opt(ref: AnyRef) = ref match {
    case null => java.util.Optional.empty()
    //   case o => java.util.Optional.of(new WrappedValue(o))
  }

  // TODO: return type
  def cast2list(ref: AnyRef): immutable.List[AnyRef] = ???

  def cast2set(ref: AnyRef): immutable.Set[AnyRef] = ref match {
    //case v: Value.Set => v.elms.map(e => new WrappedValue(e))
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  def cast2map(ref: AnyRef): immutable.Map[AnyRef, AnyRef] = ref match {
    //    case o: immutable.Map[AnyRef, AnyRef]@unchecked =>
    //      o.foldLeft(Map.empty[AnyRef, AnyRef]) {
    //        case (macc, (key, value)) =>
    //          val k = new WrappedValue(key)
    //          val v = new WrappedValue(value)
    //          macc + (k -> v)
    //      }
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }


  // TODO: Doc and cleanup.
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
  case object Unit

  /////////////////////////////////////////////////////////////////////////////
  // Bool                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  @deprecated
  final class Bool private[Value](val b: scala.Boolean) {
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
  val True: AnyRef = new Value.Bool(true)

  /**
    * The false value.
    */
  @inline
  val False: AnyRef = new Value.Bool(false)

  /**
    * Constructs a bool from the given boolean `b`.
    */
  @inline
  def mkBool(b: Boolean): AnyRef = if (b) True else False

  /**
    * Casts the given reference `ref` to a primitive boolean.
    */
  @inline
  def cast2bool(ref: Any): Boolean = ref match {
    case Value.True => true
    case Value.False => false
    case o: java.lang.Boolean => o.booleanValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-bool type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Char                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Constructs a char value from the given char `c`.
    */
  def mkChar(c: Char): AnyRef = new java.lang.Character(c)

  /**
    * Casts the given reference `ref` to a primitive char.
    */
  def cast2char(ref: AnyRef): Char = ref match {
    case o: java.lang.Character => o.charValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-char type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Ints                                                                    //
  /////////////////////////////////////////////////////////////////////////////

  /**
    * Constructs an int8 value from the given byte `b`.
    */
  def mkInt8(b: Byte): AnyRef = new java.lang.Byte(b)

  /**
    * Constructs an int8 from the given int `i`.
    */
  def mkInt8(i: scala.Int): AnyRef = new java.lang.Byte(i.asInstanceOf[Byte])

  def mkInt16(s: Short): AnyRef = ???

  def mkInt16(i: scala.Int): AnyRef = ???

  def mkInt64(i: scala.Int): AnyRef = ???

  def mkInt64(i: Long): AnyRef = ???

  /**
    * Casts the given reference `ref` to an int8.
    */
  def cast2int8(ref: AnyRef): Byte = ref match {
    case o: java.lang.Byte => o.byteValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int8 type: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int16.
    */
  def cast2int16(ref: AnyRef): Short = ref match {
    case o: java.lang.Short => o.shortValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int16 type: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int32.
    */
  def cast2int32(ref: AnyRef): scala.Int = ref match {
    case v: Value.Int => v.i
    case o: java.lang.Integer => o.intValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int32 type: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int64.
    */
  def cast2int64(ref: AnyRef): Long = ref match {
    case o: java.lang.Long => o.longValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int64 type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Closures                                                                //
  /////////////////////////////////////////////////////////////////////////////
  def cast2closure(ref: AnyRef): Closure = ref match {
    case o: Closure => o
    case _ => throw new InternalRuntimeError(s"Unexpected non-closure type: '$ref'.")
  }

  /** *************************************************************************
    * Value.Int implementation                                                *
    * **************************************************************************/

  final class Int private[Value](val i: scala.Int) {
    override val toString: java.lang.String = s"Value.Int($i)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Int => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = i.hashCode
  }

  // TODO(mhyee): Need to use weak (or soft?) references so cache doesn't grow without bound
  private val intCache = mutable.HashMap[scala.Int, Value.Int]()

  def mkInt32(i: scala.Int): AnyRef = if (intCache.contains(i)) {
    intCache(i)
  } else {
    val ret = new Value.Int(i)
    intCache(i) = ret
    ret
  }

  /** *************************************************************************
    * Value.Str implementation                                                *
    * **************************************************************************/

  final class Str private[Value](val s: java.lang.String) {
    override val toString: java.lang.String = s"Value.Str($s)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Str => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = s.hashCode
  }

  // TODO(mhyee): Need to use weak (or soft?) references so cache doesn't grow without bound
  private val strCache = mutable.HashMap[java.lang.String, Value.Str]()

  def mkStr(s: java.lang.String): AnyRef = if (strCache.contains(s)) {
    strCache(s)
  } else {
    val ret = new Value.Str(s)
    strCache(s) = ret
    ret
  }

  /** *************************************************************************
    * Value.Tag implementation                                                *
    * **************************************************************************/

  final class Tag private[Value](val enum: Name.Resolved, val tag: java.lang.String, val value: AnyRef) {
    override val toString: java.lang.String = s"Value.Tag($enum, $tag, $value)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Tag => that eq this
      case _ => false
    }

    override val hashCode: scala.Int = 41 * (41 * (41 + enum.hashCode) + tag.hashCode) + value.hashCode
  }

  // TODO(mhyee): Need to use weak (or soft?) references so cache doesn't grow without bound
  private val tagCache = mutable.HashMap[(Name.Resolved, java.lang.String, AnyRef), Value.Tag]()

  def mkTag(e: Name.Resolved, t: java.lang.String, v: AnyRef) = {
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

  final case class Tuple(elms: Array[AnyRef]) {
    override def toString: java.lang.String = s"Value.Tuple(Array(${elms.mkString(",")}))"

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Value.Tuple =>
        util.Arrays.equals(this.elms.asInstanceOf[Array[AnyRef]], that.elms.asInstanceOf[Array[AnyRef]])
      case _ => false
    }

    override def hashCode: scala.Int = util.Arrays.hashCode(elms.asInstanceOf[Array[AnyRef]])
  }

  case class Set(elms: scala.collection.immutable.Set[AnyRef])

  final case class Closure(formals: Array[String], body: TypedAst.Expression, env: mutable.Map[String, AnyRef]) {
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

  // TODO: Omit
  final case class Native(value: AnyRef)

  final case class HookClosure(inv: Invokable)

  def mkList[ValueType](list: List[ValueType]): ValueType = ??? // TODO

  def mkMap[ValueType](map: Map[ValueType, ValueType]): ValueType = ??? // TODO

  def mkNone[ValueType]: ValueType = ??? // TODO

  def mkSome[ValueType](v: ValueType): ValueType = ??? // TODO

}
