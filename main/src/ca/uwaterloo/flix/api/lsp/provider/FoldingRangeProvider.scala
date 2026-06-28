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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{FoldingRange, FoldingRangeKind}
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.ast.{Token, TokenKind}

import scala.collection.mutable

/**
  * Provides the means to handle an LSP folding range request.
  *
  * We compute folding ranges for multi-line comments so that they can be collapsed (folded) in an editor:
  *   - A run of consecutive line comments (`//`) on adjacent lines.
  *   - A run of consecutive doc comments (`///`) on adjacent lines.
  *   - A single block comment (`/* ... */`) spanning multiple lines.
  */
object FoldingRangeProvider {

  /**
    * Returns the folding ranges for the file at the given `uri`.
    */
  def getFoldingRanges(uri: String)(implicit root: Root): List[FoldingRange] = {
    root.tokens.keys.find(_.name == uri) match {
      case None => Nil
      case Some(source) =>
        val comments = root.tokens(source).iterator.filter(_.kind.isComment).toList
        foldComments(comments).sortBy(r => (r.startLine, r.endLine))
    }
  }

  /**
    * Returns a folding range for each foldable comment in the given list of `comments`.
    *
    * The `comments` must be given in source order.
    */
  private def foldComments(comments: List[Token]): List[FoldingRange] = {
    // Each block comment that spans more than one line forms its own folding range.
    val blockRanges = comments.collect {
      case tok if tok.kind == TokenKind.CommentBlock && tok.start.lineOneIndexed < tok.end.lineOneIndexed =>
        FoldingRange(tok.start.lineOneIndexed, tok.end.lineOneIndexed, FoldingRangeKind.Comment)
    }

    // A run of adjacent line comments (and likewise doc comments) forms a folding range.
    // Line and doc comments are folded separately so that the two kinds are not merged into one range.
    val lineRanges = foldRuns(comments.filter(_.kind == TokenKind.CommentLine))
    val docRanges = foldRuns(comments.filter(_.kind == TokenKind.CommentDoc))

    blockRanges ::: lineRanges ::: docRanges
  }

  /**
    * Returns a folding range for each maximal run of line-adjacent tokens in `comments` that spans at least two lines.
    *
    * The `comments` must be single-line tokens of the same kind, given in source order.
    */
  private def foldRuns(comments: List[Token]): List[FoldingRange] = {
    val ranges = mutable.ListBuffer.empty[FoldingRange]

    // The first and last token of the run that is currently being accumulated.
    var runStart: Option[Token] = None
    var runEnd: Option[Token] = None

    // Emits a folding range for the current run if it spans at least two lines.
    def closeRun(): Unit = (runStart, runEnd) match {
      case (Some(start), Some(end)) if start.start.lineOneIndexed < end.end.lineOneIndexed =>
        ranges += FoldingRange(start.start.lineOneIndexed, end.end.lineOneIndexed, FoldingRangeKind.Comment)
      case _ => // A run of a single line cannot be folded.
    }

    for (tok <- comments) {
      runEnd match {
        case Some(prev) if tok.start.lineOneIndexed == prev.end.lineOneIndexed + 1 =>
          // The token is on the line right after the previous one, so it continues the current run.
          runEnd = Some(tok)
        case _ =>
          // The token is not adjacent to the previous one, so it starts a new run.
          closeRun()
          runStart = Some(tok)
          runEnd = Some(tok)
      }
    }
    closeRun()

    ranges.toList
  }
}
