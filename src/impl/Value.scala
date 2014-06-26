package impl

trait Value

object Value {

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
  case class String(s: java.lang.String) extends Value

  /**
   * A null-ary constructor value.
   */
  case class Constructor0(name: Symbol) extends Value

  /**
   * A 1-ary constructor value.
   */
  case class Constructor1(name: Symbol, a1: Value) extends Value

  /**
   * A 2-ary constructor value.
   */
  case class Constructor2(name: Symbol, a1: Value, a2: Value) extends Value

  /**
   * A 3-ary constructor value.
   */
  case class Constructor3(name: Symbol, a1: Value, a2: Value, a3: Value) extends Value

  /**
   * A 4-ary constructor value.
   */
  case class Constructor4(name: Symbol, a1: Value, a2: Value, a3: Value, a4: Value) extends Value

  /**
   * A 5-ary constructor value.
   */
  case class Constructor5(name: Symbol, a1: Value, a2: Value, a3: Value, a4: Value, a5: Value) extends Value

}
