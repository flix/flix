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

/**
  * Represents a `FoldingRangeKind` in LSP.
  */
sealed trait FoldingRangeKind {
  def toString: String = this match {
    case FoldingRangeKind.Comment => "comment"
    case FoldingRangeKind.Imports => "imports"
    case FoldingRangeKind.Region => "region"
  }
}

/**
  * Companion object for [[FoldingRangeKind]].
  */
object FoldingRangeKind {

  case object Comment extends FoldingRangeKind

  case object Imports extends FoldingRangeKind

  case object Region extends FoldingRangeKind

}
