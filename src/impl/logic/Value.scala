package impl.logic

sealed trait Value {
  /**
   * Returns `this` value as a term with no free variables.
   */
  def asTerm: Term = Term.Constant(this)
}

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
