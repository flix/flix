/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.shared.SymUse.AssocTypeSymUse
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type}

/**
  * Represents an equality constraint appearing in a `where` clause.
  *
  * There are two forms:
  *   - [[EqualityConstraint.AssocEq]] asserts that an associated type `cst[tpe1]` equals `tpe2`.
  *   - [[EqualityConstraint.BoolEq]] asserts that two Boolean/effect formulas `tpe1` and `tpe2` are equal.
  */
sealed trait EqualityConstraint {
  def tpe1: Type

  def tpe2: Type

  def loc: SourceLocation

  /**
    * Returns this constraint with its source location replaced by `newLoc`.
    */
  def withLoc(newLoc: SourceLocation): EqualityConstraint = this match {
    case c: EqualityConstraint.AssocEq => c.copy(loc = newLoc)
    case c: EqualityConstraint.BoolEq => c.copy(loc = newLoc)
  }

  /**
    * Returns this constraint with its two types replaced by `newTpe1` and `newTpe2`.
    */
  def withTypes(newTpe1: Type, newTpe2: Type): EqualityConstraint = this match {
    case c: EqualityConstraint.AssocEq => c.copy(tpe1 = newTpe1, tpe2 = newTpe2)
    case c: EqualityConstraint.BoolEq => c.copy(tpe1 = newTpe1, tpe2 = newTpe2)
  }
}

object EqualityConstraint {

  /**
    * Represents that `symUse[tpe1]` and `tpe2` are equivalent types.
    */
  case class AssocEq(symUse: AssocTypeSymUse, tpe1: Type, tpe2: Type, loc: SourceLocation) extends EqualityConstraint

  /**
    * Represents that the Boolean/effect formulas `tpe1` and `tpe2` are equivalent.
    *
    * Both `tpe1` and `tpe2` have kind [[ca.uwaterloo.flix.language.ast.Kind.Bool]] or both have
    * kind [[ca.uwaterloo.flix.language.ast.Kind.Eff]].
    */
  case class BoolEq(tpe1: Type, tpe2: Type, loc: SourceLocation) extends EqualityConstraint

}
