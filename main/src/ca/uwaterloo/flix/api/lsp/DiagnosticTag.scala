/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j

/**
  * Represents a `DiagnosticTag` in LSP.
  */
sealed trait DiagnosticTag {
  def toInt: Int = this match {
    case DiagnosticTag.Unnecessary => 1
    case DiagnosticTag.Deprecated => 2
  }

  def toLsp4j: lsp4j.DiagnosticTag = this match {
    case DiagnosticTag.Unnecessary => lsp4j.DiagnosticTag.Unnecessary
    case DiagnosticTag.Deprecated => lsp4j.DiagnosticTag.Deprecated
  }
}

object DiagnosticTag {

  /**
    * Unused or unnecessary code.
    *
    * Clients are allowed to render diagnostics with this tag faded out instead of having an error squiggle.
    */
  case object Unnecessary extends DiagnosticTag

  /**
    * Deprecated or obsolete code.
    *
    * Clients are allowed to rendered diagnostics with this tag strike through.
    */
  case object Deprecated extends DiagnosticTag

}
