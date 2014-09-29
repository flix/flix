package impl.logic

sealed trait Type {

  /**
   * Unfolds a function type to a list of types.
   */
  def unfold: List[Type] = this match {
    case Type.Function(a, b) => a :: b.unfold
    case _ => this :: Nil
  }

  /**
   * Returns the "right-most" or "result" type of a the type.
   */
  def resultType: Type = this match {
    case Type.Function(typ1, typ2) => typ2.resultType
    case _ => this
  }

  /**
   * Returns the term where all occurences of the given variable `x` has been replaced by the type `typ`.
   */
  def substitute(x: Symbol.VariableSymbol, typ: Type): Type = this match {
    case Type.Var(s) if x == s => typ
    case Type.Var(s) => Type.Var(s)
    case Type.Unit => Type.Unit
    case Type.Bool => Type.Bool
    case Type.Int => Type.Int
    case Type.Str => Type.Str
    case Type.Set(typ1) => Type.Set(typ1.substitute(x, typ))
    case Type.Function(a, b) => Type.Function(a.substitute(x, typ), b.substitute(x, typ))
    case Type.Tag(n, typ1) => Type.Tag(n, typ1.substitute(x, typ))
    case Type.Sum(xs) => Type.Sum(xs.map(_.substitute(x, typ)))
    case Type.Tuple2(typ1, typ2) => Type.Tuple2(typ1.substitute(x, typ), typ2.substitute(x, typ))
    case Type.Tuple3(typ1, typ2, typ3) => Type.Tuple3(typ1.substitute(x, typ), typ2.substitute(x, typ), typ3.substitute(x, typ))
    case Type.Tuple4(typ1, typ2, typ3, typ4) => Type.Tuple4(typ1.substitute(x, typ), typ2.substitute(x, typ), typ3.substitute(x, typ), typ4.substitute(x, typ))
    case Type.Tuple5(typ1, typ2, typ3, typ4, typ5) => Type.Tuple5(typ1.substitute(x, typ), typ2.substitute(x, typ), typ3.substitute(x, typ), typ4.substitute(x, typ), typ5.substitute(x, typ))
  }

  /**
   * Returns the type where all occurences of variables in the given
   * environment `env` have been replaced by their respective types.
   */
  def substitute(env: Map[Symbol.VariableSymbol, Type]): Type = env.foldLeft(this) {
    case (typ1, (x, typ2)) => typ1.substitute(x, typ2)
  }
}

object Type {

  /**
   * A type variable.
   */
  case class Var(x: Symbol.VariableSymbol) extends Type

  /**
   * The type of the Unit value.
   */
  case object Unit extends Type

  /**
   * The type of booleans.
   */
  case object Bool extends Type

  /**
   * The type of integers.
   */
  case object Int extends Type

  /**
   * The type of strings.
   */
  case object Str extends Type

  /**
   * The type of sets.
   */
  case class Set(typ: Type) extends Type

  /**
   * The type of functions.
   */
  case class Function(typ1: Type, typ2: Type) extends Type

  /**
   * The type of tagged types.
   */
  case class Tag(name: Symbol.NamedSymbol, typ: Type) extends Type

  /**
   * The type of sums.
   */
  case class Sum(ts: List[Type]) extends Type

  /**
   * The type of 2-tuples.
   */
  case class Tuple2(typ1: Type, typ2: Type) extends Type

  /**
   * The type of 3-tuples.
   */
  case class Tuple3(typ1: Type, typ2: Type, typ3: Type) extends Type

  /**
   * The type of 4-tuples.
   */
  case class Tuple4(typ1: Type, typ2: Type, typ3: Type, typ4: Type) extends Type

  /**
   * The type of 5-tuples.
   */
  case class Tuple5(typ1: Type, typ2: Type, typ3: Type, typ4: Type, typ5: Type) extends Type

}
