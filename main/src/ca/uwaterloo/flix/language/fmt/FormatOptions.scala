/*
 * Copyright 2022 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.language.ast.shared.SymbolSet

/**
  * Options for formatting types.
  *
  * @param varNames the formatting style of variable names
  * @param qualify  the symbols that are written with their namespace, e.g. `A.B.Name`; every other
  *                 symbol is written by its name alone
  */
case class FormatOptions(varNames: FormatOptions.VarName, qualify: SymbolSet = SymbolSet.empty)

object FormatOptions {

  /**
    * The format options to use for internal
    */
  val Internal: FormatOptions = FormatOptions(
    varNames = VarName.IdBased
  )

  /**
    * An enum for formatting type variables.
    */
  sealed trait VarName

  object VarName {

    /**
      * Indicates that variables should be formatted by their ID, such as `t1234`.
      */
    case object IdBased extends VarName

    /**
      * Indicates that variables should be formatted by their name, such as `res`.
      */
    case object NameBased extends VarName
  }
}
