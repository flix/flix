/*
 * Copyright 2025 Gagan Chandan
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
