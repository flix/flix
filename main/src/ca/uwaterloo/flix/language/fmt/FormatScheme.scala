/*
 * Copyright 2020 Matthew Lutze, Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
        " with " + sc.tconstrs.map(FormatTraitConstraint.formatTraitConstraint).mkString(", ")

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
        " with " + sc.tconstrs.map(FormatTraitConstraint.formatTraitConstraintWithOptions(_, fmt)).mkString(", ")

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

}
