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
package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type}
import ca.uwaterloo.flix.language.fmt.{Audience, FormatType}
import ca.uwaterloo.flix.util.Formatter

sealed trait EntryPointError extends CompilationMessage {
  val kind: String = "Entry Point Error"
}

object EntryPointError {

  private implicit val audience: Audience = Audience.External

  /**
    * Error indicating an unexpected argument type to an entry point function.
    *
    * @param sym the entry point function.
    * @param tpe the argument type.
    * @param loc the location where the error occurred.
    */
  case class UnexpectedEntryPointArg(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends EntryPointError {

    override def summary: String = s"Unexpected entry point argument type: ${FormatType.formatWellKindedType(tpe)}."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The argument type: '${red(FormatType.formatWellKindedType(tpe))}' is not a valid entry point argument type.
         |
         |${code(loc, "unexpected entry point argument type.")}
         |
         |The argument type must be one of:
         |  - ${FormatType.formatWellKindedType(Type.mkArray(Type.Str, SourceLocation.Unknown))}
         |  - ${FormatType.formatWellKindedType(Type.Unit)}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Error indicating an unexpected result type to an entry point function.
    *
    * @param sym the entry point function.
    * @param tpe the result type.
    * @param loc the location where the error occurred.
    */
  case class UnexpectedEntryPointResult(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends EntryPointError {

    override def summary: String = s"Unexpected entry point result type: ${FormatType.formatWellKindedType(tpe)}."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The result type: '${red(FormatType.formatWellKindedType(tpe))}' is not a valid entry point result type.
         |
         |${code(loc, "unexpected entry point result type.")}
         |
         |The result type must be one of:
         |  - ${FormatType.formatWellKindedType(Type.Unit)}
         |  - a type with a ToString instance
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Error indicating too many arguments to an entry point function.
    *
    * @param sym the entry point function.
    * @param loc the location where the error occurred.
    */
  case class TooManyEntryPointArgs(sym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError {

    override def summary: String = s"Too many entry point arguments."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>>
         |
         |${code(loc, "too many entry point arguments.")}
         |
         |The argument type must be one of:
         |  - ${FormatType.formatWellKindedType(Type.Unit)}
         |  - a type with a ToString instance
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

}
