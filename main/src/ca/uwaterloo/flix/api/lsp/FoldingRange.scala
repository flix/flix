/*
 * Copyright 2026 Flix Authors
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
import org.json4s.*
import org.json4s.JsonDSL.*

/**
  * Represents a `FoldingRange` in LSP.
  *
  * A folding range identifies a region of text that an editor may collapse (fold) into a single line.
  *
  * We only provide line-based folding ranges, i.e. the start and end characters are left unspecified so
  * that the whole lines from [[startLine]] to [[endLine]] (inclusive) are folded.
  *
  * @param startLine The (one-indexed) line where the folded range starts.
  * @param endLine   The (one-indexed) line where the folded range ends.
  * @param kind      The kind of the folding range.
  */
case class FoldingRange(startLine: Int, endLine: Int, kind: FoldingRangeKind) {
  // NB: LSP line numbers are zero-indexed, but Flix uses one-indexed numbers internally.
  def toJSON: JValue =
    ("startLine" -> (startLine - 1)) ~
      ("endLine" -> (endLine - 1)) ~
      ("kind" -> kind.toJSON)

  // NB: LSP line numbers are zero-indexed, but Flix uses one-indexed numbers internally.
  def toLsp4j: lsp4j.FoldingRange = {
    val foldingRange = new lsp4j.FoldingRange(startLine - 1, endLine - 1)
    foldingRange.setKind(kind.toLsp4j)
    foldingRange
  }
}
