/*
 * Copyright 2022 Matthew Lutze
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

/**
  * Options for formatting types.
  *
  * @param varNames the formatting style of variable names
  */
case class FormatOptions(varNames: FormatOptions.VarName)

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
