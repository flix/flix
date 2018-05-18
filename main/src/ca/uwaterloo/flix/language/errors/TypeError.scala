/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Source, SourceLocation, Type}
import ca.uwaterloo.flix.util.InternalCompilerException
import ca.uwaterloo.flix.util.vt._
import ca.uwaterloo.flix.util.vt.VirtualString._

/**
  * A common super-type for type errors.
  */
sealed trait TypeError extends CompilationError {
  final val kind: String = "Type Error"
}

object TypeError {

  /**
    * Undefined Attribute Error.
    *
    * @param attribute the attribute name.
    * @param loc       the location where the error occurred.
    */
  // TODO: Move?
  case class UndefinedAttribute(table: String, attribute: String, loc: SourceLocation) extends TypeError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal
      vt << Line(kind, source.format) << NewLine
      vt << ">> Undefined attribute '" << Red(attribute) << "' in table '" << Cyan(table) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "attribute not found.") << NewLine
      vt << NewLine
      vt << Underline("Tip:") << " Possible typo or non-existent attribute?" << NewLine
    }
  }

  /**
    * Unification Error.
    *
    * @param baseType1 the first base type.
    * @param baseType2 the second base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param loc       the location where the error occurred.
    */
  case class UnificationError(baseType1: Type, baseType2: Type, fullType1: Type, fullType2: Type, loc: SourceLocation) extends TypeError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify '" << Red(baseType1.toString) << "' and '" << Red(baseType2.toString) << "'." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched types.") << NewLine
      vt << NewLine
      vt << "Type One: " << pretty(diff(fullType1, fullType2), Cyan) << NewLine
      vt << "Type Two: " << pretty(diff(fullType2, fullType1), Magenta) << NewLine
    }
  }

  /**
    * OccursCheck Error.
    *
    * @param baseVar   the base type variable.
    * @param baseType  the base type.
    * @param fullType1 the first full type.
    * @param fullType2 the second full type.
    * @param loc       the location where the error occurred.
    */
  case class OccursCheckError(baseVar: Type.Var, baseType: Type, fullType1: Type, fullType2: Type, loc: SourceLocation) extends TypeError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Unable to unify the type variable '" << Red(baseVar.toString) << "' with the type '" << Red(baseType.toString) << "'." << NewLine
      vt << ">> The type variable occurs recursively within the type." << NewLine
      vt << NewLine
      vt << Code(loc, "mismatched types.") << NewLine
      vt << NewLine
      vt << "Type One: " << pretty(diff(fullType1, fullType2), Cyan) << NewLine
      vt << "Type Two: " << pretty(diff(fullType2, fullType1), Magenta) << NewLine
    }
  }

  /**
    * Returns a string that represents the type difference between the two given types.
    */
  private def diff(tpe1: Type, tpe2: Type): TypeDiff = (tpe1, tpe2) match {
    case (Type.Var(_, _), _) => TypeDiff.Star(TypeConstructor.Other)
    case (_, Type.Var(_, _)) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Unit, Type.Unit) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Bool, Type.Bool) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Char, Type.Char) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Float32, Type.Float32) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Float64, Type.Float64) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Int8, Type.Int8) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Int16, Type.Int16) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Int32, Type.Int32) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Int64, Type.Int64) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.BigInt, Type.BigInt) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Str, Type.Str) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Native, Type.Native) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Array, Type.Array) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Vector, Type.Vector)  => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Zero, Type.Zero) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Succ(n1, t1), Type.Succ(n2, t2)) => TypeDiff.Star(TypeConstructor.Other)
    case (Type.Arrow(l1), Type.Arrow(l2)) if l1 == l2 => TypeDiff.Star(TypeConstructor.Arrow)
    case (Type.Enum(name1, kind1), Type.Enum(name2, kind2)) if name1 == name2 => TypeDiff.Star(TypeConstructor.Enum(name1.name))
    case (Type.Tuple(l1), Type.Tuple(l2)) if l1 == l2 => TypeDiff.Star(TypeConstructor.Tuple)
    case (Type.Apply(t11, t12), Type.Apply(t21, t22)) =>
      (diff(t11, t21), diff(t12, t22)) match {
        case (TypeDiff.Star(_), TypeDiff.Star(_)) => TypeDiff.Star(TypeConstructor.Other)
        case (diff1, diff2) => TypeDiff.Apply(diff1, diff2)
      }
    case _ => TypeDiff.Mismatch(tpe1, tpe2)
  }

  /**
    * A common super-type for type differences.
    */
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
    case class Star(constructor: TypeConstructor) extends TypeDiff

    /**
      * Represents a type application.
      */
    case class Apply(tpe1: TypeDiff, tpe2: TypeDiff) extends TypeDiff

    /**
      * Represents two mismatched types.
      */
    case class Mismatch(tpe1: Type, tpe2: Type) extends TypeDiff

  }

  /**
    * Represents a type constructor.
    */
  sealed trait TypeConstructor

  object TypeConstructor {

    /**
      * Arrow constructor.
      */
    case object Arrow extends TypeConstructor

    /**
      * Enum constructor.
      */
    case class Enum(name: String) extends TypeConstructor

    /**
      * Tuple constructor.
      */
    case object Tuple extends TypeConstructor

    /**
      * Other constructor.
      */
    case object Other extends TypeConstructor

  }

  /**
    * Returns a human readable representation of the given type difference.
    */
  private def pretty(td: TypeDiff, color: String => VirtualString): VirtualTerminal = {
    val vt = new VirtualTerminal()

    def visit(d: TypeDiff): Unit = {
      val base = d.typeConstructor
      val args = d.typeArguments

      base match {
        case TypeDiff.Star(constructor) => constructor match {
          case TypeConstructor.Arrow =>
            intercalate(args, visit, vt, before = "", separator = " -> ", after = "")
          case TypeConstructor.Enum(name) =>
            vt << name
            intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
          case TypeConstructor.Tuple =>
            intercalate(args, visit, vt, before = "(", separator = ", ", after = ")")
          case TypeConstructor.Other =>
            vt << "*"
            intercalate(args, visit, vt, before = "[", separator = ", ", after = "]")
        }
        case TypeDiff.Mismatch(tpe1, tpe2) => vt << color(tpe1.toString)
        case _ => throw InternalCompilerException(s"Unexpected base type: '$base'.")
      }
    }

    visit(td)

    vt
  }

  /**
    * Helper function to generate text before, in the middle of, and after a list of items.
    */
  private def intercalate[A](xs: List[A], f: A => Unit, vt: VirtualTerminal, before: String, separator: String, after: String): Unit = {
    if (xs.isEmpty) return
    vt << before
    var first: Boolean = true
    for (x <- xs) {
      if (first) {
        f(x)
      } else {
        vt << separator
        f(x)
      }
      first = false
    }
    vt << after
  }

}