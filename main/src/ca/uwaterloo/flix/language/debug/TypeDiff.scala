package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.Type

sealed trait TypeDiff {

  /**
    * Returns the type constructor of `this` type.
    */
  def typeConstructor: TypeDiff = this match {
    case TypeDiff.Star(_) => this
    case TypeDiff.Mismatch(t1, t2) => this
    case TypeDiff.Apply(t1, _) => t1.typeConstructor
  }

  /**
    * Returns the type parameters of `this` type.
    */
  def typeArguments: List[TypeDiff] = this match {
    case TypeDiff.Star(_) => Nil
    case TypeDiff.Mismatch(t1, t2) => Nil
    case TypeDiff.Apply(t1, t2) => t1.typeArguments ::: t2 :: Nil
  }

}

object TypeDiff {

  /**
    * Represents a matched type.
    */
  case class Star(constructor: TyCon) extends TypeDiff

  /**
    * Represents a type application.
    */
  case class Apply(tpe1: TypeDiff, tpe2: TypeDiff) extends TypeDiff

  /**
    * Represents two mismatched types.
    */
  case class Mismatch(tpe1: Type, tpe2: Type) extends TypeDiff

  /**
    * Returns a string that represents the type difference between the two given types.
    */
  def diff(tpe1: Type, tpe2: Type): TypeDiff = (tpe1, tpe2) match {
    case (Type.Var(_, _, _), _) => TypeDiff.Star(TyCon.Other)
    case (_, Type.Var(_, _, _)) => TypeDiff.Star(TyCon.Other)
    case (Type.Cst(tc1), Type.Cst(tc2)) if tc1 == tc2 => TypeDiff.Star(TyCon.Other)
    case (Type.Zero, Type.Zero) => TypeDiff.Star(TyCon.Other)
    case (Type.Succ(n1, t1), Type.Succ(n2, t2)) => TypeDiff.Star(TyCon.Other)
    case (Type.Arrow(l1, _), Type.Arrow(l2, _)) if l1 == l2 => TypeDiff.Star(TyCon.Arrow)
    case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
      (diff(t11, t21), diff(t12, t22)) match {
        case (TypeDiff.Star(_), TypeDiff.Star(_)) => TypeDiff.Star(TyCon.Other)
        case (diff1, diff2) => TypeDiff.Apply(diff1, diff2)
      }
    case _ => TypeDiff.Mismatch(tpe1, tpe2)
  }

  /**
    * Represents a type constructor.
    */
  sealed trait TyCon

  object TyCon {

    /**
      * Arrow constructor.
      */
    case object Arrow extends TyCon

    /**
      * Enum constructor.
      */
    case class Enum(name: String) extends TyCon

    /**
      * Tuple constructor.
      */
    case object Tuple extends TyCon

    /**
      * Other constructor.
      */
    case object Other extends TyCon

  }
}

