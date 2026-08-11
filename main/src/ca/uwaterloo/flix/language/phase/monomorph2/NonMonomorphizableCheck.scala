/*
 * Copyright 2026 Simon Lykke Andersen
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

package ca.uwaterloo.flix.language.phase.monomorph2

import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation}
import ca.uwaterloo.flix.util.{Graph, InternalCompilerException}

/**
  * The purpose of this phase is to reject non-monomorphizable programs (i.e. programs with
  * polymorphic recursion). This is needed to ensure that the subsequent [[ConstraintSolver]]
  * phase does not attempt to create a solution that will grow without bound.
  *
  * Following "The Simple Essence of Monomorphization" (§3.3.2), the collection of [[FlowConstraint]]s
  * is reinterpreted as a graph where edges are annotated with whether they are growing or not.
  *
  * Take the following program containing polymorphic recursion:
  * {{{
  *  enum PerfectTree[a] {
  *      case Leaf(a)
  *      case Node(PerfectTree[(a, a)])
  *  }
  *  def size(t: PerfectTree[b]): Int32 = match t {
  *     case PerfectTree.Leaf(_)     => 1
  *     case PerfectTree.Node(inner) => 2 * size(inner)
  *  }
  *  def main(): Unit \ IO = {
  *      let t = PerfectTree.Node(PerfectTree.Leaf((1, 2)));
  *      println(size(t))
  *  }
  * }}}
  * The problematic [[FlowConstraint]]s for this program are (with nicer formatting):
  * {{{
  *   [(a,a)] ~> a // Stemming from `case Node(PerfectTree[(a, a)])`
  *   [(b,b)] ~> b // Stemming from the recursive `size(inner)` call
  * }}}
  * These are problematic because they establish a cycle (self-loop) with a "growing"
  * edge. Where "growing" means that the type-variable in the cycle is nested in another
  * type. When we reinterpret the [[FlowConstraint]]s of a program with polymorphic
  * recursion as a graph there will be at least one cycle with some "growing" edge.
  *
  * N.B. Because of [[TreeShaker1]], unreachable occurrences of polymorphic recursive function
  * definitions (`def`, `sig`, instance `def`) will not be detected, whereas unreachable
  * polymorphic recursive enum/struct declarations will still be rejected.
  */
object NonMonomorphizableCheck {

  /** One tracked slot: the `pos`'th type-parameter position of `mvar`. */
  private case class Vertex(mvar: MonoVar, pos: Int)

  /** A graph edge: `src` flows into `dst`, `growing` iff it does so wrapped in a type constructor. */
  private case class Edge(src: Vertex, dst: Vertex, growing: Boolean)

  // TODO Make it a proper compiler error-message
  /** Checks whether `flows` contains a growing cycle and throws [[InternalCompilerException]] if so. */
  def checkMonomorphizable(flows: List[FlowConstraint]): Unit = {
    val edges = for {
      FlowConstraint(Instantiation(args), dst) <- flows
      (arg, i) <- args.zipWithIndex
      (v, j) <- MonoArg.collectParams(arg).distinct
    } yield Edge(Vertex(v, j), Vertex(dst, i), growing = isGrowingHead(arg))

    val adjacency = edges.groupMap(_.src)(_.dst)
    val getAdj = (v: Vertex) => adjacency.getOrElse(v, Nil)
    val vertices = edges.iterator.flatMap(e => Iterator(e.src, e.dst)).toSet
    val scc = Graph.stronglyConnectedComponents(vertices, getAdj)

    // Detect polymorphic recursion: If a growing edge is on a cycle (i.e. endpoints share SCC).
    edges.find(e => e.growing && scc(e.src) == scc(e.dst)) match {
      case Some(edge) =>
        throw InternalCompilerException(
          s"Program is not monomorphizable: found an infinitely-growing recursive type " +
          s"involving ${edge.src.mvar}. This indicates polymorphic recursion " +
          s"(e.g. `def f(x: a): List[a] = ...f(lst)...`) or a genuinely non-regular recursive " +
          s"enum/struct (e.g. `enum T[a] { ...case Recurse(T[List[a]])... }`) — Flix " +
          s"cannot generate a finite number of monomorphized copies for this definition.",
          monoVarLoc(edge.src.mvar)
        )
      case None => ()
    }
  }

  /**
    * Returns `true` iff `arg`'s outermost wrapping is growing.
    */
  private def isGrowingHead(arg: MonoArg): Boolean = arg match {
    // a direct copy, never growth
    case MonoArg.Param(_, _)                => false
    // set algebra doesn't count as nesting
    case MonoArg.App(MonoArg.Const(tpe), _) =>
      Kind.resultKind(tpe.kind) match {
        case Kind.Eff        => false
        case Kind.Bool       => false
        case Kind.CaseSet(_) => false
        case _               => true
      }
    case MonoArg.App(_, _)                  => true
    case MonoArg.Const(_)                   => true
    case MonoArg.Assoc(_, _, _, _)          => true
  }

  /** Returns the source location of `mvar`'s declaration. */
  private def monoVarLoc(mvar: MonoVar): SourceLocation = mvar match {
    case MonoVar.Def(sym)              => sym.loc
    case MonoVar.Enum(sym)             => sym.loc
    case MonoVar.Sig(sym)              => sym.loc
    case MonoVar.RestrictableEnum(sym) => sym.loc
    case MonoVar.Struct(sym)           => sym.loc
  }
}
