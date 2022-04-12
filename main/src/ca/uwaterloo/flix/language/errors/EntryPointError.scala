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
    * Error indicating one or more arguments to an entry point function.
    *
    * @param sym the entry point function.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointArgs(sym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError {

    override def summary: String = s"Unexpected entry point argument(s)."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Arguments to the entry point function are not permitted.
         |
         |${code(loc, "unexpected entry point argument(s).")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = None
  }

  /**
    * Error indicating an illegal result type to an entry point function.
    *
    * @param sym the entry point function.
    * @param tpe the result type.
    * @param loc the location where the error occurred.
    */
  case class IllegalEntryPointResult(sym: Symbol.DefnSym, tpe: Type, loc: SourceLocation) extends EntryPointError {

    override def summary: String = s"Unexpected entry point result type: ${FormatType.formatWellKindedType(tpe)}."

    override def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> The result type: '${red(FormatType.formatWellKindedType(tpe))}' is not a valid entry point result type.
         |
         |${code(loc, "unexpected entry point result type.")}
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |The result type must be one of:
         |
         |  (1) ${FormatType.formatWellKindedType(Type.Unit)}
         |  (2) a type with a ToString instance
         |
         |""".stripMargin
    })
  }

  /**
    * Error indicating the specified entry point is missing.
    *
    * @param sym the entry point function.
    * @param loc the location where the error occurred.
    */
  case class EntryPointNotFound(sym: Symbol.DefnSym, loc: SourceLocation) extends EntryPointError {
    override def summary: String = s"Entry point ${sym} not found."

    override def message(formatter: Formatter): String = {
      s""">> The entry point ${sym} cannot be found.
         |""".stripMargin
    }

    override def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |Possible fixes:
         |
         |  (1)  Change the specified entry point to an existing function.
         |  (2)  Add an entry point function ${sym}.
         |
         |""".stripMargin
    })
  }
}
