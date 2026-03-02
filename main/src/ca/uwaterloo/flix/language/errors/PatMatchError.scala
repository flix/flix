/*
 * Copyright 2025 Flix Contributors
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
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.fmt.FormatConstant
import ca.uwaterloo.flix.language.phase.PatMatch2.WitnessPattern
import ca.uwaterloo.flix.util.Formatter

/** A common super-type for pattern match errors. */
sealed trait PatMatchError extends CompilationMessage {
  val kind: CompilationMessageKind = CompilationMessageKind.PatternMatchError
}

object PatMatchError {

  /**
    * An error raised to indicate a non-exhaustive pattern match expression.
    *
    * @param pat the witness pattern that is not covered.
    * @param loc the location of the match expression.
    */
  case class NonExhaustiveMatch(pat: WitnessPattern, loc: SourceLocation) extends PatMatchError {
    def code: ErrorCode = ErrorCode.E5952

    def summary: String = s"Non-exhaustive match: missing case '${formatPattern(pat)}'."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      val patStr = formatPattern(pat)
      s""">> Non-exhaustive match: missing case '${red(patStr)}'.
         |
         |${highlight(loc, "missing case", fmt)}
         |""".stripMargin
    }
  }

  /**
    * An error raised to indicate a redundant (unreachable) pattern in a match expression.
    *
    * @param coveredBy the location of the first pattern that covers this one, if a single such pattern exists.
    * @param loc       the location of the redundant pattern.
    */
  case class RedundantPattern(coveredBy: Option[SourceLocation], loc: SourceLocation) extends PatMatchError {
    def code: ErrorCode = ErrorCode.E5963

    def summary: String = "Unreachable pattern: already covered by a preceding case."

    def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
      import fmt.*
      coveredBy match {
        case None =>
          s""">> ${red("Unreachable pattern: already covered by a preceding case.")}
             |
             |${highlight(loc, "unreachable case", fmt)}
             |""".stripMargin
        case Some(cb) =>
          s""">> ${red("Unreachable pattern: already covered by a preceding case.")}
             |
             |${highlight(loc, "unreachable case", fmt)}
             |
             |Covered by the following pattern:
             |
             |${highlight(cb, "covering pattern", fmt)}
             |""".stripMargin
      }
    }
  }

  /**
    * Formats a `WitnessPattern` as a human-readable string for error messages.
    */
  private def formatPattern(wp: WitnessPattern): String = wp match {
    case WitnessPattern.Wildcard => "_"

    case WitnessPattern.Literal(cst) => FormatConstant.formatConstant(cst)

    case WitnessPattern.Tag(sym, Nil) => sym.name

    case WitnessPattern.Tag(sym, args) =>
      sym.name + args.map(formatPattern).mkString("(", ", ", ")")

    case WitnessPattern.Tuple(elms) =>
      elms.map(formatPattern).mkString("(", ", ", ")")

    case WitnessPattern.Record(fields) =>
      if (fields.isEmpty)
        "{ }"
      else
        fields.map { case (name, p) => s"$name = ${formatPattern(p)}" }.mkString("{ ", ", ", " }")
  }

}
