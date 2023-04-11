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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Scheme

object FormatScheme {

  /**
    * Construct a string representation of the type scheme,  e.g.
    * `∀(a, b).a -> Int -> b with Show[a], Eq[b]`
    */
  def formatScheme(sc: Scheme)(implicit flix: Flix): String = {
    val mainPart = formatSchemeWithoutConstraints(sc)

    val tconstrPart =
      if (sc.tconstrs.isEmpty)
        ""
      else
        " with " + sc.tconstrs.map(FormatTypeConstraint.formatTypeConstraint).mkString(", ")

    val econstrPart =
      if (sc.econstrs.isEmpty)
        ""
      else
        " where " + sc.econstrs.map(FormatEqualityConstraint.formatEqualityConstraint).mkString(", ")

    mainPart + tconstrPart + econstrPart
  }

  /**
    * Construct a string representation of the type scheme,  e.g.
    * `∀(a, b).a -> Int -> b with Show[a], Eq[b]`
    */
  def formatSchemeWithOptions(sc: Scheme, fmt: FormatOptions): String = {
    val mainPart = formatSchemeWithoutConstraintsWithOptions(sc, fmt)

    val tconstrPart =
      if (sc.tconstrs.isEmpty)
        ""
      else
        " with " + sc.tconstrs.map(FormatTypeConstraint.formatTypeConstraintWithOptions(_, fmt)).mkString(", ")

    val econstrPart =
      if (sc.econstrs.isEmpty)
        ""
      else
        " where " + sc.econstrs.map(FormatEqualityConstraint.formatEqualityConstraintWithOptions(_, fmt)).mkString(", ")

    mainPart + tconstrPart + econstrPart
  }

  /**
    * Construct a string representation of the type scheme, excluding type constraints, e.g.,
    * `∀(a, b).a -> Int -> b`
    */
  def formatSchemeWithoutConstraints(sc: Scheme)(implicit flix: Flix): String = {
    formatSchemeWithoutConstraintsWithOptions(sc, flix.getFormatOptions)
  }

  /**
    * Construct a string representation of the type scheme, excluding type constraints, e.g.,
    * `∀(a, b).a -> Int -> b`
    */
  def formatSchemeWithoutConstraintsWithOptions(sc: Scheme, fmt: FormatOptions): String = {
    val quantifiersPart =
      if (sc.quantifiers.isEmpty)
        ""
      else
        "∀(" + sc.quantifiers.map(FormatType.formatTypeVarSymWithOptions(_, fmt)).mkString(", ") + "). "

    val typePart = FormatType.formatTypeWithOptions(sc.base, fmt)

    quantifiersPart + typePart
  }

  /**
    * Construct a string representation of the type scheme, including equality constraints but excluding type constraints, e.g.,
    * `∀(a, b).a -> Int -> b where Elem[a] ~ String`
    */
  def formatSchemeWithOnlyEqualityConstraints(sc: Scheme)(implicit flix: Flix): String = {
    val fmt = flix.getFormatOptions
    // TODO ASSOC-TYPES just make a helper for each "part" and call them
    val quantifiersPart =
      if (sc.quantifiers.isEmpty)
        ""
      else
        "∀(" + sc.quantifiers.map(FormatType.formatTypeVarSymWithOptions(_, fmt)).mkString(", ") + "). "

    val typePart = FormatType.formatTypeWithOptions(sc.base, fmt)

    val econstrPart =
      if (sc.econstrs.isEmpty)
        ""
      else
        " where " + sc.econstrs.map(FormatEqualityConstraint.formatEqualityConstraintWithOptions(_, fmt)).mkString(", ")

    quantifiersPart + typePart + econstrPart
  }
}
