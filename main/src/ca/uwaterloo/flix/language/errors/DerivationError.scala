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
    * Illegal type class derivation for an empty enum
    *
    * @param clazz The type class being derived
    * @param loc   The location where the error occured
    */
  case class IllegalDerivationForEmptyEnum(`enum`: Symbol.EnumSym, clazz: Symbol.ClassSym, classLoc: SourceLocation)(implicit flix: Flix) extends DerivationError {
    def summary: String = s"Illegal type class derivation for empty enum ${`enum`.toString}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""
         |>> Illegal type class derivation for an empty enum
         |
         |Attempted to derive an instance of ${cyan(clazz.toString)} for enum ${cyan(`enum`.toString)} but it is empty
         |
         |${code(classLoc, "illegal type class derivation")}
         |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some("Empty enums cannot derive type classes")

    def loc = classLoc
  }
}
