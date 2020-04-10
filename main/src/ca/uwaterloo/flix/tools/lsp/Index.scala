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
package ca.uwaterloo.flix.tools.lsp

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
// TODO: Currently ignores the document
case class Index(m: Map[Int, List[(Int, Expression)]]) {

  // TODO: DOC
  def query(doc: Document, pos: Position): Option[Expression] = {
    // TODO: Currently ignores the document

    //val candidates = m.getOrElse(line, Nil).filter(p => p._1 >= column)
    // TODO: Compute some notion of span. candidates.sortBy()
    None
  }

  /**
    * Adds the given expression `exp0` to `this` index.
    */
  def +(exp0: Expression): Index = {
    val beginLine = exp0.loc.beginLine
    val beginCol = exp0.loc.beginCol
    val onLine = m.getOrElse(beginLine, Nil)
    val newOnLine = (beginCol, exp0) :: onLine
    val m2 = m + (beginLine -> newOnLine)
    Index(m2)
  }

  /**
    * Merges two indexes.
    */
  def ++(that: Index): Index = {
    val m3 = that.m.foldLeft(this.m) {
      case (macc, (line, onLine2)) =>
        val onLine = macc.getOrElse(line, Nil)
        val result = onLine ::: onLine2
        macc + (line -> result)
    }
    Index(m3)
  }

}