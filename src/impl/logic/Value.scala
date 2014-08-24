package impl.logic

import impl.runtime.Error

sealed trait Value {
  /**
   * Returns `this` value as a term. Always succeeds.
   */
  def toTerm: Term = this match {
    case Value.Bool(b) => Term.Bool(b)
    case Value.Int(i) => Term.Int(i)
    case Value.Str(s) => Term.Str(s)
    case Value.Constructor0(s) => Term.Constructor0(s)
    case Value.Constructor1(s, v1) => Term.Constructor1(s, v1.toTerm)
    case Value.Constructor2(s, v1, v2) => Term.Constructor2(s, v1.toTerm, v2.toTerm)
    case Value.Constructor3(s, v1, v2, v3) => Term.Constructor3(s, v1.toTerm, v2.toTerm, v3.toTerm)
    case Value.Constructor4(s, v1, v2, v3, v4) => Term.Constructor4(s, v1.toTerm, v2.toTerm, v3.toTerm, v4.toTerm)
    case Value.Constructor5(s, v1, v2, v3, v4, v5) => Term.Constructor5(s, v1.toTerm, v2.toTerm, v3.toTerm, v4.toTerm, v5.toTerm)
  }

  /**
   * Returns `this` value as a boolean or throws a type error.
   */
  def toBool: Boolean = this match {
    case Value.Bool(b) => b
    case _ => throw Error.TypeError(Type.Bool, this)
  }

  /**
   * Returns `this` value as an int or throws a type error.
   */
  def toInt: Int = this match {
    case Value.Int(i) => i
    case _ => throw Error.TypeError(Type.Bool, this)
  }

  /**
   * Returns `this` value as a string or throws a type error.
   */
  def toStr: String = this match {
    case Value.Str(s) => s
    case _ => throw Error.TypeError(Type.Bool, this)
  }

}

object Value {

  /**
   * The unit value.
   */
  case object Unit extends Value

  /**
   * A boolean value.
   */
  case class Bool(b: scala.Boolean) extends Value

  /**
   * An integer value.
   */
  case class Int(i: scala.Int) extends Value

  /**
   * A string value.
   */
  case class Str(s: java.lang.String) extends Value

  /**
   * A tagged value.
   */
  case class Tagged(name: Symbol.NamedSymbol, v: Value) extends Value

  /**
   * A 2-tuple value.
   */
  case class Tuple2(v1: Value, v2: Value) extends Value

  /**
   * A 3-tuple value.
   */
  case class Tuple3(v1: Value, v2: Value, v3: Value) extends Value

  /**
   * A 4-tuple value.
   */
  case class Tuple4(v1: Value, v2: Value, v3: Value, v4: Value) extends Value

  /**
   * A 5-tuple value.
   */
  case class Tuple5(v1: Value, v2: Value, v3: Value, v4: Value, v5: Value) extends Value

  /**
   * A null-ary constructor value.
   */
  case class Constructor0(name: Symbol.NamedSymbol) extends Value

  /**
   * A 1-ary constructor value.
   */
  case class Constructor1(name: Symbol.NamedSymbol, v1: Value) extends Value

  /**
   * A 2-ary constructor value.
   */
  case class Constructor2(name: Symbol.NamedSymbol, v1: Value, v2: Value) extends Value

  /**
   * A 3-ary constructor value.
   */
  case class Constructor3(name: Symbol.NamedSymbol, v1: Value, v2: Value, v3: Value) extends Value

  /**
   * A 4-ary constructor value.
   */
  case class Constructor4(name: Symbol.NamedSymbol, v1: Value, v2: Value, v3: Value, v4: Value) extends Value

  /**
   * A 5-ary constructor value.
   */
  case class Constructor5(name: Symbol.NamedSymbol, v1: Value, v2: Value, v3: Value, v4: Value, v5: Value) extends Value

}
