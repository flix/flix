/*
 * Copyright 2025 Gagan Chandan
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.lsp

import org.eclipse.lsp4j
import org.json4s.JValue
import org.json4s.JsonDSL.*

/**
  * Represents `DiagnosticRelatedInformation` in LSP.
  */
case class DiagnosticRelatedInformation(location: Location, message: String) {
  def toJSON: JValue =
    ("location" -> location.toJSON) ~
      ("message" -> message)

  def toLsp4j: lsp4j.DiagnosticRelatedInformation = {
    val relatedInfo = new lsp4j.DiagnosticRelatedInformation()
    relatedInfo.setLocation(location.toLsp4j)
    relatedInfo.setMessage(message)
    relatedInfo
  }
}
