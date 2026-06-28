/*
 * Copyright 2026 Magnus Madsen
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
  * Represents a `FoldingRangeKind` in LSP.
  *
  * A set of predefined kinds that describe the meaning of a [[FoldingRange]].
  */
sealed trait FoldingRangeKind {
  def toJSON: JValue = this match {
    case FoldingRangeKind.Comment => "comment"
    case FoldingRangeKind.Imports => "imports"
    case FoldingRangeKind.Region => "region"
  }

  def toLsp4j: String = this match {
    case FoldingRangeKind.Comment => lsp4j.FoldingRangeKind.Comment
    case FoldingRangeKind.Imports => lsp4j.FoldingRangeKind.Imports
    case FoldingRangeKind.Region => lsp4j.FoldingRangeKind.Region
  }
}

object FoldingRangeKind {

  /**
    * A folding range for a comment.
    */
  case object Comment extends FoldingRangeKind

  /**
    * A folding range for imports or includes.
    */
  case object Imports extends FoldingRangeKind

  /**
    * A folding range for a region (e.g. `#region`).
    */
  case object Region extends FoldingRangeKind

}
