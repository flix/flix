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

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.ast.TypedAst.Expression

object Index {
  /**
    * Represents the empty reverse index.
    */
  val empty: Index = Index(Map.empty)

  /**
    * Returns the index of the given expression `exp0`.
    */
  def of(exp0: Expression): Index = empty + exp0
}

/**
  * Represents a reserve index from documents to line numbers to expressions.
  */
case class Index(m: Map[(String, Int), List[Expression]]) {

  /**
    * Optionally returns the expression in the document at the given `uri` at the given position `pos`.
    */
  // TODO: Add support for multi-line expressions.
  def query(uri: String, pos: Position): Option[Expression] = {
    // A key consists of a uri and a line number.
    val key = (uri, pos.line)
    m.get(key).flatMap {
      case candidates =>
        // We have all expressions on that uri and on that line.

        // Step 1: Compute all whole range overlap with the given position.
        val filtered = candidates.filter(e => e.loc.beginCol <= pos.col && pos.col <= e.loc.endCol)

        // Step 2: Sort the expressions by their span (i.e. their length).
        val sorted = filtered.sortBy(e => span(e.loc))

        // Print all candidates.
        // println(sorted.map(_.loc.format).mkString("\n"))

        // Step 3: Return the candidate with the smallest span.
        sorted.headOption
    }
  }

  /**
    * Adds the given expression `exp0` to `this` index.
    */
  def +(exp0: Expression): Index = {
    // Compute the uri, line, and column of the expression.
    val uri = exp0.loc.source.name
    val beginLine = exp0.loc.beginLine
    val beginCol = exp0.loc.beginCol

    // Compute the other expressions already on that uri and line.
    val otherExps = m.getOrElse((uri, beginLine), Nil)

    // Prepend the current expression to the other expressions on that uri and line.
    val newExps = exp0 :: otherExps

    // Returns an updated map.
    Index(m + ((uri, beginLine) -> newExps))
  }

  /**
    * Merges two indexes.
    */
  def ++(that: Index): Index = {
    val m3 = that.m.foldLeft(this.m) {
      case (macc, (line, exps1)) =>
        val exps2 = macc.getOrElse(line, Nil)
        val result = exps1 ::: exps2
        macc + (line -> result)
    }
    Index(m3)
  }

  /**
    * Returns the span (i.e. length) of the given source location `loc`.
    */
  private def span(loc: SourceLocation): Int =
    if (loc.beginLine == loc.endLine)
      loc.endCol - loc.beginCol
    else
      1000 // TODO: Add support for multi-line expressions.

}