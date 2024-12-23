/*
 * Copyright 2024 Holger Dal Mogensen
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
package ca.uwaterloo.flix.language.ast.shared

import ca.uwaterloo.flix.language.ast.shared.LabelledPrecedenceGraph.{Label, LabelledEdge}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, Type}

/**
  * Represents a labelled graph; the dependency graph with additional labels
  * on the edges allowing more accurate filtering. The rule `A :- not B, C` would
  * add dependency edges `B -x> A` and `C -> A`. The labelled graph can then
  * add labels that allow the two edges to be filtered out together. If we
  * look at a program consisting of A, B, and D. then the rule `C -> A`
  * cannot be relevant, but by remembering that B occurred together with A,
  * we can also rule out `B -x> A`. The labelled edges would be `B -[C]-x> A`
  * and `C -[B]-> A`.
  */
object LabelledPrecedenceGraph {
  /**
    * Represents a positive or negative labelled dependency edge.
    *
    * The labels represent predicate nodes that must co-occur for the dependency to be relevant.
    */
  case class LabelledEdge(head: Name.Pred, polarity: Polarity, fixity: Fixity, labels: Vector[Label], body: Name.Pred, loc: SourceLocation)

  /**
    * Represents a label in the labelled graph.
    */
  case class Label(pred: Name.Pred, den: Denotation, arity: Int, terms: List[Type])

  /**
    * The empty labelled graph.
    */
  val empty: LabelledPrecedenceGraph = LabelledPrecedenceGraph(Vector.empty)
}

case class LabelledPrecedenceGraph(edges: Vector[LabelledEdge]) {
  /**
    * Returns a labelled graph with all labelled edges in `this` and `that` labelled graph.
    */
  def +(that: LabelledPrecedenceGraph): LabelledPrecedenceGraph = {
    if (this eq LabelledPrecedenceGraph.empty)
      that
    else if (that eq LabelledPrecedenceGraph.empty)
      this
    else
      LabelledPrecedenceGraph(this.edges ++ that.edges)
  }

  /**
    * Returns `this` labelled graph including only the edges where all its labels are in
    * `syms` and the labels match according to `'`labelEq`'`.
    *
    * A rule like `A(ta) :- B(tb), not C(tc).` is represented by `edge(A, pos, {la, lb, lc}, B)` etc.
    * and is only included in the output if `syms` contains all of `la.pred, lb.pred, lc.pred` and `labelEq(syms(A), la)` etc.
    */
  def restrict(syms: Map[Name.Pred, Label], labelEq: (Label, Label) => Boolean): LabelledPrecedenceGraph = {
    def include(l: Label): Boolean = syms.get(l.pred).exists(l2 => labelEq(l, l2))

    LabelledPrecedenceGraph(edges.filter {
      case LabelledEdge(_, _, _, labels, _, _) => labels.forall(include)
    })
  }
}
