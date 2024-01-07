/*
 * Copyright 2017 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.Ast.SyntacticContext
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter

/**
  * An error raised to indicate a parse error.
  *
  * @param msg the error message.
  * @param ctx the syntactic context.
  * @param loc the source location.
  */
case class ParseError(msg: String, ctx: SyntacticContext, loc: SourceLocation) extends CompilationMessage with Unrecoverable {
  val kind = "Parse Error"

  def summary: String = msg

  def message(formatter: Formatter): String = {
    import formatter._
    s"""${line(kind, source.name)}
       |>> Parse Error: ${red(msg)}
       |
       |Syntactic Context: $ctx.
       |""".stripMargin
  }
}
