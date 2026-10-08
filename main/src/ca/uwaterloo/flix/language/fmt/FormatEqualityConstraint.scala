/*
 * Copyright 2023 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.EqualityConstraint

object FormatEqualityConstraint {

  /**
    * Formats the given `econstr` as `Assoc[Arg] ~ Type`.
    */
  def formatEqualityConstraint(econstr: EqualityConstraint)(implicit flix: Flix): String = {
    formatEqualityConstraintWithOptions(econstr, flix.getFormatOptions)
  }

  /**
    * Formats the given `econstr` as `Assoc[Arg] ~ Type`.
    */
  def formatEqualityConstraintWithOptions(tconstr: EqualityConstraint, fmt: FormatOptions): String = tconstr match {
    case EqualityConstraint(symUse, tpe1, tpe2, _) =>
      val assocString = symUse.sym.name
      val tpe1String = FormatType.formatTypeWithOptions(tpe1, fmt)
      val tpe2String = FormatType.formatTypeWithOptions(tpe2, fmt)
      s"$assocString[$tpe1String] ~ $tpe2String"
  }
}
