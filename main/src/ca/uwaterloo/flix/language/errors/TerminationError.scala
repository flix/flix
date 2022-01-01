/*
 * Copyright 2021 Matthew Lutze
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
  * A common super-type for termination errors.
  */
sealed trait TerminationError extends CompilationMessage {
  val kind: String = "Termination Error"
}

object TerminationError {

  /**
    * An error raised to indicate that the given definition recurses unconditionally.
    *
    * @param sym the unconditionally recursive definition.
    */
  case class UnconditionalRecursion(sym: Symbol.DefnSym) extends TerminationError {
    def summary: String = "Unconditional recursion."

    def message(formatter: Formatter): String = {
      import formatter._
      s"""${line(kind, source.name)}
         |>> Unconditionally recursive definition '${red(sym.name)}'. All branches will recurse indefinitely.
         |
         |${code(sym.loc, "unconditional recursion.")}
         |
         |""".stripMargin
    }

    def loc: SourceLocation = sym.loc

    def explain(formatter: Formatter): Option[String] = Some({
      s"""
         |"Possible fixes:"
         |
         |  (1)  Add a non-recursive branch to the definition.
         |
         |""".stripMargin
    })
  }
}
