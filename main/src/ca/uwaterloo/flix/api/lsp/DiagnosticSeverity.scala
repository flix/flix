/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.errors.Severity
import org.eclipse.lsp4j

/**
  * Represents a `DiagnosticSeverity` in LSP.
  */
sealed trait DiagnosticSeverity {
  def toInt: Int = this match {
    case DiagnosticSeverity.Error => 1
    case DiagnosticSeverity.Warning => 2
    case DiagnosticSeverity.Information => 3
    case DiagnosticSeverity.Hint => 4
  }

  def toLsp4j: lsp4j.DiagnosticSeverity = this match {
    case DiagnosticSeverity.Error => lsp4j.DiagnosticSeverity.Error
    case DiagnosticSeverity.Warning => lsp4j.DiagnosticSeverity.Warning
    case DiagnosticSeverity.Information => lsp4j.DiagnosticSeverity.Information
    case DiagnosticSeverity.Hint => lsp4j.DiagnosticSeverity.Hint
  }
}

object DiagnosticSeverity {

  /**
    * Returns the [[DiagnosticSeverity]] corresponding to the given [[Severity]].
    */
  def from(s: Severity): DiagnosticSeverity = s match {
    case Severity.Error => Error
    case Severity.Info => Information
    case Severity.Hint => Hint
  }

  /**
    * Reports an error.
    */
  case object Error extends DiagnosticSeverity

  /**
    * Reports a warning.
    */
  case object Warning extends DiagnosticSeverity

  /**
    * Reports an information.
    */
  case object Information extends DiagnosticSeverity

  /**
    * Reports a hint.
    */
  case object Hint extends DiagnosticSeverity

}
