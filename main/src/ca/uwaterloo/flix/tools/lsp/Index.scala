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
  val Empty: Index = Index(Map.empty)

  def of(e: Expression): Index = Empty // TODO
}

case class Index(m: Map[Int, List[(Int, Expression)]]) {

  def query(doc: Document, pos: Position): Option[Expression] = {
    //val candidates = m.getOrElse(line, Nil).filter(p => p._1 >= column)
    // TODO: Compute some notion of span. candidates.sortBy()
    None
  }

  def ++(that: Index): Index = this // TODO

  def +(e: Expression): Index = Index.Empty // TODO
}