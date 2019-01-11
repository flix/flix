package ca.uwaterloo.flix.language.ast

/**
  * Representation of mono types.
  */
sealed trait MonoType {

  @deprecated("will be removed", "0.5")
  def typeArguments: List[MonoType] = this match {
    case MonoType.Array(tpe) => List(tpe)
    case MonoType.Channel(tpe) => List(tpe)
    case MonoType.Enum(_, targs) => targs
    case MonoType.Arrow(targs, tresult) => targs ::: tresult :: Nil
    case MonoType.Tuple(elms) => elms
    case MonoType.Ref(tpe) => List(tpe)
    case MonoType.Apply(tpe1, tpe2) => tpe1.typeArguments ::: tpe2 :: Nil
    case _ => Nil
  }

}

object MonoType {

  ///
  /// Primitive Types.
  ///

  case object Unit extends MonoType

  case object Bool extends MonoType

  case object Char extends MonoType

  case object Float32 extends MonoType

  case object Float64 extends MonoType

  case object Int8 extends MonoType

  case object Int16 extends MonoType

  case object Int32 extends MonoType

  case object Int64 extends MonoType

  case object BigInt extends MonoType

  case object Str extends MonoType

  ///
  /// Compound Types.
  ///

  case class Array(tpe: MonoType) extends MonoType

  case class Channel(tpe: MonoType) extends MonoType

  case class Ref(tpe: MonoType) extends MonoType

  case class Tuple(elms: List[MonoType]) extends MonoType

  case class Enum(sym: Symbol.EnumSym, args: List[MonoType]) extends MonoType // TODO: We want elms here?

  case class Arrow(args: List[MonoType], result: MonoType) extends MonoType

  case class RecordEmpty() extends MonoType

  case class RecordExtend(label: String, value: MonoType, rest: MonoType) extends MonoType

  case class Relation(sym: Symbol.RelSym, attr: List[MonoType]) extends MonoType

  case class Lattice(sym: Symbol.LatSym, attr: List[MonoType]) extends MonoType

  case class Schema(m: Map[Symbol.PredSym, MonoType]) extends MonoType

  case class Native(clazz: Class[_]) extends MonoType

  /**
    * A type expression that a type application tpe1[tpe2].
    */
  @deprecated("will be removed", "0.5")
  case class Apply(tpe1: MonoType, tpe2: MonoType) extends MonoType

  @deprecated("will be removed", "0.5")
  case class Var(id: Int) extends MonoType

}
