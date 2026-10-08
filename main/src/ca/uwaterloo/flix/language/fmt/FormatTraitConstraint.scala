/*
 * Copyright 2021 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.TraitConstraint

object FormatTraitConstraint {

  /**
    * Formats the given `tconstr` as `Class[Param]`.
    */
  def formatTraitConstraint(tconstr: TraitConstraint)(implicit flix: Flix): String = {
    formatTraitConstraintWithOptions(tconstr, flix.getFormatOptions)
  }

  /**
    * Formats the given `tconstr` as `Class[Param]`.
    */
  def formatTraitConstraintWithOptions(tconstr: TraitConstraint, fmt: FormatOptions): String = tconstr match {
    case TraitConstraint(head, arg, _) =>
      val typeString = FormatType.formatTypeWithOptions(arg, fmt)
      s"${head.sym}[$typeString]"
  }
}
