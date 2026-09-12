/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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
