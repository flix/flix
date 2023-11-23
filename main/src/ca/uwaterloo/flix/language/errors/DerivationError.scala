/*
 * Copyright 2022 Paul Butcher
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for derivation errors.
  */
sealed trait DerivationError extends CompilationMessage {
  val kind: String = "Derivation Error"
}

object DerivationError {

  /**
    * Illegal type class derivation for an empty enum.
    *
    * @param sym      the enum symbol.
    * @param classSym the class symbol of what is being derived.
    * @param loc      The source location where the error occurred.
    */
  case class IllegalDerivationForEmptyEnum(sym: Symbol.EnumSym, classSym: Symbol.ClassSym, loc: SourceLocation)(implicit flix: Flix) extends DerivationError {
    def summary: String = s"Cannot derive '${classSym.name}' for the empty enum '${sym.name}'."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Cannot derive '${magenta(classSym.name)}' for the empty enum '${red(sym.name)}'.
         |
         |${code(loc, "illegal derivation")}
         |
         |Flix cannot derive any instances for an empty enumeration.
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = None
  }

}
