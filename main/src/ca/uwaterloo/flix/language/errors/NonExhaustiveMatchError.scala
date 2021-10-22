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

import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}

/**
 * An error raised to indicate a non exhaustive pattern match expression.
 */
case class NonExhaustiveMatchError(rules: List[TypedAst.MatchRule], pat: String, loc: SourceLocation) extends CompilationMessage {
  val kind = "Pattern Match"

  def summary: String = s"Non-exhaustive match. Missing case: '$pat'."

  def message: String = {
    s"""${Format.line(kind, source.format)}
       |>> Non-Exhaustive Pattern. Missing case: ${Format.red(pat)} in match expression.
       |
       |${Format.code(loc, "incomplete pattern.")}
       |""".stripMargin

  }
}
