package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}

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

  def diff(tpe1: Type, tpe2: Type): TypeDiff = {
    val tyCon1 = tpe1.typeConstructor
    val tyCon2 = tpe2.typeConstructor

    (tyCon1, tyCon2) match {
      case (Type.Var(_, _, _), _) => TypeDiff.Star(TyCon.Other)
      case (_, Type.Var(_, _, _)) => TypeDiff.Star(TyCon.Other)
      case (Type.Cst(TypeConstructor.Tuple(len1)), Type.Cst(TypeConstructor.Tuple(len2))) if (len1 == len2) =>
        val diffs = (tpe1.typeArguments zip tpe2.typeArguments).map { case (t1, t2) => diff(t1, t2) }
        mkApply(TypeDiff.Star(TyCon.Tuple), diffs)
      case (Type.Cst(TypeConstructor.Enum(sym1, kind1)), Type.Cst(TypeConstructor.Enum(sym2, kind2))) if ((sym1 == sym2) && (kind1 == kind2)) =>
        val diffs = (tpe1.typeArguments zip tpe2.typeArguments).map { case (t1, t2) => diff(t1, t2) }
        mkApply(TypeDiff.Star(TyCon.Enum(sym1.name)), diffs)
      case (Type.Cst(tc1), Type.Cst(tc2)) if tc1 == tc2 => TypeDiff.Star(TyCon.Other)
      case (Type.Arrow(len1, _), Type.Arrow(len2, _)) if (len1 == len2) =>
        val diffs = (tpe1.typeArguments zip tpe2.typeArguments).map { case (t1, t2) => diff(t1, t2) }
        mkApply(TypeDiff.Star(TyCon.Arrow), diffs)
      case _ => TypeDiff.Mismatch(tpe1, tpe2)
    }
  }

  private def mkApply(base: TypeDiff, params: List[TypeDiff]): TypeDiff = {
    params.foldLeft(base)((base0, param) => TypeDiff.Apply(base0, param))
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

