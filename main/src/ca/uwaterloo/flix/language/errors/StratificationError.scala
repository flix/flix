/*
 *  Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.{CompilationMessage, CompilationMessageKind}
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.errors.Highlighter.highlight
import ca.uwaterloo.flix.language.fmt.FormatType
import ca.uwaterloo.flix.util.Formatter

/**
  * An error raised to indicate that a constraint set is not stratified.
  */
case class StratificationError(cycle: List[(Name.Pred, SourceLocation)], tpe: Type, loc: SourceLocation)(implicit flix: Flix) extends CompilationMessage {
  def kind: CompilationMessageKind = CompilationMessageKind.StratificationError

  def code: ErrorCode = ErrorCode.E5914

  def summary: String = "The expression is not stratified. A predicate depends strongly on itself."

  def message(fmt: Formatter)(implicit root: Option[TypedAst.Root]): String = {
    import fmt.*
    s""">> The expression is not stratified. A predicate depends strongly on itself.
       |
       |${highlight(loc, "the expression is not stratified.", fmt)}
       |
       |The type of the expression is:
       |
       |  ${cyan(FormatType.formatType(tpe))}
       |
       |The following predicate symbols are on the cycle:
       |
       |  ${cycle.map(_._1).mkString(" <- ")}
       |
       |The following constraints are part of the cycle:
       |${fmtConstraints(fmt)}
       |""".stripMargin
  }

  /**
    * Formats the constraint dependencies.
    */
  private def fmtConstraints(fmt: Formatter): String = {
    cycle.map(t => "  " + fmt.cyan(t._1.name) + " at " + t._2.format + " (which depends on)" + System.lineSeparator()).mkString
  }
}
