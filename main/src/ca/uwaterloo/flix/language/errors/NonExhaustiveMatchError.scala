/*
 * Copyright 2017 Jason Mitterteiner
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

import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.Formatter

/**
  * An error raised to indicate a non-exhaustive pattern match expression.
  */
case class NonExhaustiveMatchError(pat: String, loc: SourceLocation) extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.PatternMatchError

  def code: ErrorCode = ErrorCode.E5952

  def summary: String = s"Non-exhaustive match: missing case '$pat'."

  def message(formatter: Formatter): String = {
    import formatter.*
    s""">> Non-exhaustive match: missing case '${red(pat)}'.
       |
       |${src(loc, "incomplete match.")}
       |
       |${underline("Explanation:")} Every match expression must be exhaustive, i.e. cover all
       |possible cases. A wildcard pattern can be used to handle remaining cases. For example:
       |
       |    case _ => // handle all other cases.
       |""".stripMargin
  }

}
