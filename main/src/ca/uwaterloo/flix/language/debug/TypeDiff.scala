/*
 * Copyright 2020 Matthew Lutze, Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.debug

import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor}

sealed trait TypeDiff {

  /**
    * Returns the type constructor of `this` type.
    */
  def typeConstructor: TypeDiff = this match {
    case TypeDiff.Apply(t1, _) => t1.typeConstructor
    case _ => this
  }

  /**
    * Returns the type parameters of `this` type.
    */
  def typeArguments: List[TypeDiff] = this match {
    case TypeDiff.Apply(t1, t2) => t1.typeArguments ::: t2 :: Nil
    case _ => Nil
  }

}

object TypeDiff {

  /**
    * Represents a tuple.
    */
  case object Tuple extends TypeDiff

  /**
    * Represents an enum.
    */
  case object Enum extends TypeDiff

  /**
    * Represents an arrow type.
    */
  case object Arrow extends TypeDiff

  /**
    * Represents a miscellaneous type.
    */
  case object Other extends TypeDiff

  /**
    * Represents a type application.
    */
  case class Apply(tpe1: TypeDiff, tpe2: TypeDiff) extends TypeDiff

  /**
    * Represents two mismatched types.
    */
  case class Mismatch(tpe1: Type, tpe2: Type) extends TypeDiff

  /**
    * Construct a TypeDiff from the given types.
    *
    * Recursively searches the type structure, identifying mismatches between the two types. The resulting TypeDiff has
    * the same shape as the given types as far as they are identical, but replacing differences with a `Mismatch`.
    *
    * Does not handle schema or record types as their equality requires different logic.
    *
    * Examples:
    *   * `diff(Int32, Int32) => Other`
    *   * `diff((Int32, Int32), (Int32, Bool)) => Tuple[Other, Mismatch(Int32, Bool)]`
    *   * `diff(Int32 -> Int32, Int32 -> Bool) => Arrow[Other, Mismatch(Int32, Bool)]`
    *   * `diff(Option[Int32], Option[Bool]) => Enum[Mismatch(Int32, Bool)]`
    */
  def diff(tpe1: Type, tpe2: Type): TypeDiff = {
    val tyCon1 = tpe1.typeConstructor
    val tyCon2 = tpe2.typeConstructor

    (tyCon1, tyCon2) match {
      case (Type.Var(_, _, _), _) => TypeDiff.Other
      case (_, Type.Var(_, _, _)) => TypeDiff.Other
      case (Type.Cst(TypeConstructor.Tuple(len1)), Type.Cst(TypeConstructor.Tuple(len2))) if (len1 == len2) =>
        val diffs = (tpe1.typeArguments zip tpe2.typeArguments).map { case (t1, t2) => diff(t1, t2) }
        mkApply(TypeDiff.Tuple, diffs)
      case (Type.Cst(TypeConstructor.Enum(sym1, kind1)), Type.Cst(TypeConstructor.Enum(sym2, kind2))) if ((sym1 == sym2) && (kind1 == kind2)) =>
        val diffs = (tpe1.typeArguments zip tpe2.typeArguments).map { case (t1, t2) => diff(t1, t2) }
        mkApply(TypeDiff.Enum, diffs)
      case (Type.Cst(tc1), Type.Cst(tc2)) if tc1 == tc2 => TypeDiff.Other
      case (Type.Arrow(len1, _), Type.Arrow(len2, _)) if (len1 == len2) =>
        val diffs = (tpe1.typeArguments zip tpe2.typeArguments).map { case (t1, t2) => diff(t1, t2) }
        mkApply(TypeDiff.Arrow, diffs)
      case _ => TypeDiff.Mismatch(tpe1, tpe2)
    }
  }

  private def mkApply(base: TypeDiff, params: List[TypeDiff]): TypeDiff = {
    params.foldLeft(base)((base0, param) => TypeDiff.Apply(base0, param))
  }
}

