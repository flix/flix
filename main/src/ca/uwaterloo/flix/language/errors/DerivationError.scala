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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol}
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for derivation errors.
  */
sealed trait DerivationError extends CompilationMessage {
  val kind: String = "Derivation Error"
}

object DerivationError {

  /**
    * An error indicating an illegal functor shape.
    *
    * @param enum the symbol of the enum.
    * @param loc  the location where the error occurred.
    */
  case class IllegalFunctorShape(enum: Symbol.EnumSym, loc: SourceLocation) extends DerivationError {
    override def summary: String = "Illegal functor shape"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Illegal functor shape.
         |
         |${code(loc, s"illegal functor shape for enum ${enum.name}")}
         |
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      import formatter._
      s"${underline("Tip:")} a Functor must have at least one type parameter."
    })
  }
}
