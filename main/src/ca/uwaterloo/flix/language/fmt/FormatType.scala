/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.shared.SymbolSet
import ca.uwaterloo.flix.language.ast.{RigidityEnv, Symbol, Type}

/**
  * The string entry points over [[Pretty]].
  */
object FormatType {
  /**
    * Transforms the given well-kinded type into a string.
    *
    * Minimizes the given type.
    *
    * Performs alpha renaming if the rigidity environment is present.
    */
  def formatType(tpe: Type, renv: Option[RigidityEnv] = None, minimizeEffs: Boolean = false, amb: SymbolSet = SymbolSet.empty)(implicit flix: Flix): String =
    guarded(Pretty.tpe(tpe, renv, minimizeEffs)(FormatOptions.Default.copy(qualify = amb)))

  /**
    * Transforms the given well-kinded type into a string, using the given format options.
    */
  def formatTypeWithOptions(tpe: Type, fmt: FormatOptions): String =
    guarded(Pretty.tpe(tpe)(fmt))

  /**
    * Transforms the given kinded type variable symbol into a string.
    */
  def formatTypeVarSymWithOptions(sym: Symbol.KindedTypeVarSym, fmt: FormatOptions): String =
    Pretty.typeVar(sym)(fmt).plain

  /**
    * Transforms the given type view into a string.
    */
  def formatDisplayType(tpe: DisplayType)(implicit flix: Flix): String =
    formatDisplayTypeWithOptions(tpe, FormatOptions.Default)

  /**
    * Transforms the given type view into a string, using the given format options.
    */
  def formatDisplayTypeWithOptions(tpe: DisplayType, fmt: FormatOptions): String =
    Pretty.displayType(tpe)(fmt).plain

  /**
    * Renders the given tree as plain text, or returns a marker if it cannot be built.
    */
  private def guarded(f: => Fmt): String = {
    try {
      f.plain
    } catch {
      case _: Throwable => "ERR_UNABLE_TO_FORMAT_TYPE"
    }
  }

}
