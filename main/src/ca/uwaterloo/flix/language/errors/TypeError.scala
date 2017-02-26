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
import ca.uwaterloo.flix.language.ast.{SourceInput, SourceLocation, Type}
import ca.uwaterloo.flix.util.vt._
import ca.uwaterloo.flix.util.vt.VirtualString._

/**
  * A common super-type for type errors.
  */
// TODO: Make sealed
// TODO: Move kind here.
trait TypeError extends CompilationError

object TypeError {

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
    val kind = "Type Error"
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Unable to unify ").quote(Red(baseType1.toString)).text(" and ").quote(Red(baseType2.toString)).text(".").newLine().
      newLine().
      highlight(loc, "mismatched types.").newLine().
      newLine().
      text("Type One: ").use(pretty(diff(fullType1, fullType2), Cyan)).newLine().
      text("Type Two: ").use(pretty(diff(fullType2, fullType1), Magenta)).newLine()
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
    val kind = "Type Error"
    val source: SourceInput = loc.source
    val message: VirtualTerminal = new VirtualTerminal().
      header(kind, source).
      text(">> Unable to unify the type variable ").quote(Red(baseVar.toString)).text(" with the type ").quote(Red(baseType.toString)).text(".").newLine().
      text(">> due to a recursive occurrence of the type variable in the type.").newLine().
      newLine().
      highlight(loc, "mismatched types.").newLine().
      newLine().
      text("Type One: ").use(pretty(diff(fullType1, fullType2), Cyan)).newLine().
      text("Type Two: ").use(pretty(diff(fullType2, fullType1), Magenta)).newLine()
  }

  /**
    * Returns a string that represents the type difference between the two given types.
    */
  private def diff(tpe1: Type, tpe2: Type): TypeDiff = (tpe1, tpe2) match {
    case (Type.Var(_, _), _) => TypeDiff.Star
    case (_, Type.Var(_, _)) => TypeDiff.Star
    case (Type.Unit, Type.Unit) => TypeDiff.Star
    case (Type.Bool, Type.Bool) => TypeDiff.Star
    case (Type.Char, Type.Char) => TypeDiff.Star
    case (Type.Float32, Type.Float32) => TypeDiff.Star
    case (Type.Float64, Type.Float64) => TypeDiff.Star
    case (Type.Int8, Type.Int8) => TypeDiff.Star
    case (Type.Int16, Type.Int16) => TypeDiff.Star
    case (Type.Int32, Type.Int32) => TypeDiff.Star
    case (Type.Int64, Type.Int64) => TypeDiff.Star
    case (Type.BigInt, Type.BigInt) => TypeDiff.Star
    case (Type.Str, Type.Str) => TypeDiff.Star
    case (Type.Native, Type.Native) => TypeDiff.Star
    case (Type.Arrow(l1), Type.Arrow(l2)) if l1 == l2 => TypeDiff.Star
    case (Type.FTuple(l1), Type.FTuple(l2)) if l1 == l2 => TypeDiff.Star
    case (Type.Enum(name1, kind1), Type.Enum(name2, kind2)) if name1 == name2 => TypeDiff.Star
    case (Type.Apply(Type.Arrow(l1), ts1), Type.Apply(Type.Arrow(l2), ts2)) =>
      TypeDiff.Arrow(diffAll(ts1, ts2))
    case (Type.Apply(Type.FTuple(l1), ts1), Type.Apply(Type.FTuple(l2), ts2)) =>
      TypeDiff.Tuple(diffAll(ts1, ts2))
    case _ => TypeDiff.Error(tpe1, tpe2)
  }

  /**
    * Returns a string that represents the type difference between the two given type lists.
    */
  private def diffAll(ts1: List[Type], ts2: List[Type]): List[TypeDiff] = (ts1, ts2) match {
    case (Nil, Nil) => Nil
    case (Nil, rs2) => Nil
    case (rs1, Nil) => rs1.map(_ => TypeDiff.Missing)
    case (t1 :: rs1, t2 :: rs2) => diff(t1, t2) :: diffAll(rs1, rs2)
  }

  /**
    * A common super-type for type differences.
    */
  sealed trait TypeDiff

  object TypeDiff {

    case object Star extends TypeDiff

    case object Missing extends TypeDiff

    case class Arrow(ts: List[TypeDiff]) extends TypeDiff

    case class Tuple(ts: List[TypeDiff]) extends TypeDiff

    case class Error(tpe1: Type, tpe2: Type) extends TypeDiff

  }

  /**
    * Returns a human readable representation of the given type difference.
    */
  private def pretty(td: TypeDiff, color: String => VirtualString): (VirtualTerminal) => Unit = {
    message => {
      def visit(d: TypeDiff): Unit = d match {
        case TypeDiff.Star => message.text("...")
        case TypeDiff.Missing => message.text("???")
        case TypeDiff.Arrow(xs) =>
          message.text("(")
          xs.init.foreach(visit)
          message.text(")")
          message.text(" -> ")
          visit(xs.last)
        case TypeDiff.Tuple(xs) =>
          message.text("(")
          xs.foreach(visit)
          message.text(")")
        case TypeDiff.Error(tpe1, tpe2) => message.text(color(tpe1.toString))
      }

      visit(td)
    }
  }

}