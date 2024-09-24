/*
 * Copyright 2020 Magnus Madsen
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

/** Represents a `DiagnosticTag` in LSP. */
sealed trait DiagnosticTag {
  def toInt: Int = this match {
    case DiagnosticTag.Unnecessary => 1
    case DiagnosticTag.Deprecated => 2
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
