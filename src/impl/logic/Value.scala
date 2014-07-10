package impl.logic

sealed trait Value {
  /**
   * Returns the value as a term (with no free variables).
   */
  def asTerm: Term = this match {
    case Value.Bool(b) => Term.Bool(b)
    case Value.Int(i) => Term.Int(i)
    case Value.String(s) => Term.String(s)
    case Value.Constructor0(s) => Term.Constructor0(s)
    case Value.Constructor1(s, v1) => Term.Constructor1(s, v1.asTerm)
    case Value.Constructor2(s, v1, v2) => Term.Constructor2(s, v1.asTerm, v2.asTerm)
    case Value.Constructor3(s, v1, v2, v3) => Term.Constructor3(s, v1.asTerm, v2.asTerm, v3.asTerm)
    case Value.Constructor4(s, v1, v2, v3, v4) => Term.Constructor4(s, v1.asTerm, v2.asTerm, v3.asTerm, v4.asTerm)
    case Value.Constructor5(s, v1, v2, v3, v4, v5) => Term.Constructor5(s, v1.asTerm, v2.asTerm, v3.asTerm, v4.asTerm, v5.asTerm)
  }
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
