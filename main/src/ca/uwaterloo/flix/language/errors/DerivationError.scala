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
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.fmt.FormatType.formatType
import ca.uwaterloo.flix.language.fmt._
import ca.uwaterloo.flix.util.Formatter

/**
  * A common super-type for derivation errors.
  */
sealed trait DerivationError extends CompilationMessage {
  val kind: String = "Derivation Error"
}

object DerivationError {
  implicit val audience = Audience.External

  /**
    * Unable to derive Immutable error
    *
    * @param enum0 the enum for which we're trying to derive Immutable
    * @param loc   the location where the error occurred.
    */
  case class ImmutableError(enum0: KindedAst.Enum, loc: SourceLocation) extends DerivationError {
    def summary: String = s"Cannot derive Immutable for enum ${enum0}"

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
        |>> Cannot derive 'Immutable' for enum ${red(enum0.sym.toString())}
        |
        |Because it takes a type parameter of kind 'Region'.
        |
        |${code(loc, "unable to derive Immutable.")}
        |
        |""".stripMargin
    }

    def explain(formatter: Formatter): Option[String] = Some(
      s"""
        |An example of a type parameter of kind 'Region':
        |
        |enum MyEnum[r: Region] { ... }
        |""".stripMargin
    )
  }
}
