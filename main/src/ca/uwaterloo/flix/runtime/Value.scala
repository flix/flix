package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.runtime.Interpreter.InternalRuntimeError
import scala.collection.immutable
import scala.collection.mutable

import java.util

object Value {

  /////////////////////////////////////////////////////////////////////////////
  // Unit                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The Unit value.
    */
  object Unit

  /////////////////////////////////////////////////////////////////////////////
  // Bools                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * The true value.
    */
  @inline
  val True: AnyRef = java.lang.Boolean.TRUE

  /**
    * The false value.
    */
  @inline
  val False: AnyRef = java.lang.Boolean.FALSE

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
    case o: java.lang.Boolean => o.booleanValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-bool type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Chars                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Constructs a char value from the given char `c`.
    */
  @inline
  def mkChar(c: Char): AnyRef = new java.lang.Character(c)

  /**
    * Casts the given reference `ref` to a primitive char.
    */
  @inline
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
  @inline
  def mkInt8(b: Byte): AnyRef = new java.lang.Byte(b)

  /**
    * Constructs an int8 from the given int `i`.
    */
  @inline
  def mkInt8(i: Int): AnyRef = new java.lang.Byte(i.asInstanceOf[Byte])

  /**
    * Constructs an int16 from the given short `s`.
    */
  @inline
  def mkInt16(s: Short): AnyRef = new java.lang.Short(s)

  /**
    * Constructs an int16 form the given int `i`.
    */
  @inline
  def mkInt16(i: Int): AnyRef = new java.lang.Short(i.asInstanceOf[Short])

  /**
    * Constructs an int32 from the given int `i`.
    */
  @inline
  def mkInt32(i: Int): AnyRef = new java.lang.Integer(i)

  /**
    * Constructs an int64 from the given int `i`.
    */
  @inline
  def mkInt64(i: Int): AnyRef = new java.lang.Long(i)

  /**
    * Constructs an int64 from the given long `l`.
    */
  @inline
  def mkInt64(l: Long): AnyRef = new java.lang.Long(l)

  /**
    * Casts the given reference `ref` to an int8.
    */
  @inline
  def cast2int8(ref: AnyRef): Byte = ref match {
    case o: java.lang.Byte => o.byteValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int8 type: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int16.
    */
  @inline
  def cast2int16(ref: AnyRef): Short = ref match {
    case o: java.lang.Short => o.shortValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int16 type: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int32.
    */
  @inline
  def cast2int32(ref: AnyRef): Int = ref match {
    case o: java.lang.Integer => o.intValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int32 type: '$ref'.")
  }

  /**
    * Casts the given reference `ref` to an int64.
    */
  @inline
  def cast2int64(ref: AnyRef): Long = ref match {
    case o: java.lang.Long => o.longValue()
    case _ => throw new InternalRuntimeError(s"Unexpected non-int64 type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Closures                                                                //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Flix internal representation of closures.
    */
  final case class Closure(formals: Array[String], body: TypedAst.Expression, env: mutable.Map[String, AnyRef]) {
    // TODO: Why override equals?
    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Value.Closure =>
        util.Arrays.equals(this.formals.asInstanceOf[Array[AnyRef]], that.formals.asInstanceOf[Array[AnyRef]]) &&
          this.body == that.body && this.env == that.env
      case _ => false
    }

    override def hashCode: Int =
      41 * (41 * (41 + util.Arrays.hashCode(formals.asInstanceOf[Array[AnyRef]])) + body.hashCode) + env.hashCode
  }

  // TODO: Introduce make function and make Closure constructor private.

  /**
    * Casts the given reference `ref` to a closure.
    */
  @inline
  def cast2closure(ref: AnyRef): Closure = ref match {
    case o: Closure => o
    case _ => throw new InternalRuntimeError(s"Unexpected non-closure type: '$ref'.")
  }

  /**
    * Flix internal representation of hook closures.
    */
  final case class HookClosure(inv: Invokable)

  // TODO: introduce make function and make class private?

  /////////////////////////////////////////////////////////////////////////////
  // Strings                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Constructs a str from the given string `s`.
    */
  @inline
  def mkStr(s: String): AnyRef = s.intern()

  /**
    * Casts the given reference `ref` to a string.
    */
  @inline
  def cast2str(ref: AnyRef): String = ref match {
    case o: java.lang.String => o
    case _ => throw new InternalRuntimeError(s"Unexpected non-string type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tags                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Flix internal representation of tags.
    */
  // TODO: Technically we don't need to store the enum name with the tag.
  final class Tag private[Value](val enum: Name.Resolved, val tag: java.lang.String, val value: AnyRef) {
    override val toString: java.lang.String = s"Value.Tag($enum, $tag, $value)"

    override def equals(other: Any): scala.Boolean = other match {
      case that: Value.Tag => that eq this
      case _ => false
    }

    override val hashCode: Int = 41 * (41 * (41 + enum.hashCode) + tag.hashCode) + value.hashCode
  }

  /**
    * A cache for every tag ever created.
    */
  private val tagCache = mutable.HashMap[(Name.Resolved, java.lang.String, AnyRef), Value.Tag]()

  /**
    * Constructs the tag for the given enum name `e`, tag name `t` and tag value `v`.
    */
  @inline
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

  /**
    * Casts the given reference `ref` to a tag.
    */
  @inline
  def cast2tag(ref: AnyRef): Value.Tag = ref match {
    case v: Value.Tag => v
    case _ => throw new InternalRuntimeError(s"Unexpected non-tag type: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Tuples                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Remove
  final case class Tuple(elms: Array[AnyRef]) {
    override def toString: java.lang.String = s"Value.Tuple(Array(${elms.mkString(",")}))"

    override def equals(obj: scala.Any): Boolean = obj match {
      case that: Value.Tuple =>
        util.Arrays.equals(this.elms.asInstanceOf[Array[AnyRef]], that.elms)
      case _ => false
    }

    override def hashCode: Int = util.Arrays.hashCode(elms.asInstanceOf[Array[AnyRef]])
  }

  /**
    * Casts the given reference `ref` to a tuple.
    */
  @inline
  def cast2tuple(ref: AnyRef): Array[AnyRef] = ref match {
    // case v: Value.Tuple => v.elms.map(e => new WrappedValue(e))
    //case o: Array[AnyRef] => o.map(e => new WrappedValue(e))
    case _ => throw new UnsupportedOperationException(s"Unexpected value: '$ref'.")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Opt, List, Set, Map                                                     //
  /////////////////////////////////////////////////////////////////////////////
  /**
    * Constructs the `None` value.
    */
  @inline
  def mkNone: AnyRef = null

  /**
    * Constructs the `Some` value for the given `ref`.
    */
  @inline
  def mkSome(ref: AnyRef): AnyRef = ref

  /**
    * Constructs the flix representation of a list for the given `ref`.
    */
  def mkList(ref: AnyRef): AnyRef = ??? // TODO



  def mkSet(ref: AnyRef): AnyRef = ref match {
    case o: immutable.Set[_] => o
    case o: java.lang.Iterable[_] => ???

  }


  def mkMap[ValueType](map: Map[ValueType, ValueType]): ValueType = ??? // TODO


  // TODO: return type
  def cast2opt(ref: AnyRef) = ref match {
    case null => java.util.Optional.empty()
    //   case o => java.util.Optional.of(new WrappedValue(o))
  }

  // TODO: return type
  def cast2list(ref: AnyRef): immutable.List[AnyRef] = ???


  def cast2set(ref: AnyRef): Traversable[AnyRef] = ref match {
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

  /////////////////////////////////////////////////////////////////////////////
  // Pretty Printing                                                         //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Doc and cleanup.
  def pretty(o: AnyRef): String = o match {
    case Value.Unit => "()"

    case v: Value.Tag => s"${v.enum}.${v.tag}(${pretty(v.value)})"
    case Value.Tuple(elms) => "(" + elms.map(pretty).mkString(",") + ")"
    case _ => o.toString
  }

}
