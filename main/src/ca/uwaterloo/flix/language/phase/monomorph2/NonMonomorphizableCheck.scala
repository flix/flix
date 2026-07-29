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

import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor}
import ca.uwaterloo.flix.util.{Graph, InternalCompilerException}

/**
  * Rejects non-monomorphizable programs (flow sets with no finite solution) before
  * [[ConstraintSolver.solve]]'s fixpoint loop, which would otherwise grow without bound.
  *
  * Following "The Simple Essence of Monomorphization" (§3.3.2), the flow set is reinterpreted as
  * a graph over `(MonoVar, tuple-position)` vertices, and we look for a "growing cycle": a reachable
  * cycle with at least one edge that wraps the flowing type in an additional type constructor
  * (`a` flowing into `List[a]`). Such a cycle arises from polymorphic recursion
  * (e.g. `def f(x: a): List[a] = f(x::Nil)`). A cycle of only direct-copy edges is
  * ordinary, convergent self-recursion and is fine.
  *
  * The check over-approximates in one known way: a non-regular enum whose growing case is declared
  * but never constructed is still rejected once any other case of it is constructed, even though
  * demand-driven specialization would not need the growing case.
  */
object NonMonomorphizableCheck {

  /** One tracked slot: the `pos`'th type-parameter position of `mvar`. */
  private case class Vertex(mvar: MonoVar, pos: Int)

  /** A graph edge: `src` flows into `dst`, `growing` iff it does so wrapped in a type constructor. */
  private case class Edge(src: Vertex, dst: Vertex, growing: Boolean)

  // TODO Make it a proper compiler error-message
  /**
    * Checks whether `flows` contains a reachable growing cycle and throws
    * [[InternalCompilerException]] if so.
    */
  def checkMonomorphizable(flows: Set[Flow]): Unit = {
    val positions = flows.iterator.flatMap {
      case Flow(args, dst) =>
        args.iterator.zipWithIndex.map { case (arg, i) =>
          val dstV = Vertex(dst, i)
          (arg, dstV, MonoArg.collectParams(arg).distinct)
        }
    }.toList

    val seeds = positions.collect { case (_, dstV, ps) if ps.isEmpty => dstV }
    val edges = positions.flatMap { case (arg, dstV, ps) =>
      ps.map { case (v, j) => Edge(Vertex(v, j), dstV, growing = isGrowingHead(arg)) }
    }

    if (edges.isEmpty) return

    val adjacency = edges.groupMap(_.src)(_.dst)
    val getAdj = (v: Vertex) => adjacency.getOrElse(v, Nil)
    val reachable = Graph.reachable(seeds, getAdj)
    val vertices = edges.iterator.flatMap(e => Iterator(e.src, e.dst)).toSet
    val scc = Graph.stronglyConnectedComponents(vertices, getAdj)

    // Detect polymorphic recursion: If a growing reachable edge is on a cycle (i.e. endpoints share SCC).
    edges.find(e => e.growing && reachable(e.src) && scc(e.src) == scc(e.dst)) match {
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
    case MonoArg.Param(_, _) => false
    // set algebra doesn't count as nesting
    case MonoArg.App(MonoArg.Const(Type.Cst(tc, _)), _) => !isSetAlgebraConstructor(tc)
    case _ => true
  }

  /** Returns `true` iff `tc` is an effect, case-set, or Boolean formula algebra constructor. */
  private def isSetAlgebraConstructor(tc: TypeConstructor): Boolean = tc match {
    case TypeConstructor.Union | TypeConstructor.Intersection | TypeConstructor.Complement |
         TypeConstructor.Difference | TypeConstructor.SymmetricDiff => true
    case TypeConstructor.CaseUnion(_) | TypeConstructor.CaseIntersection(_) |
         TypeConstructor.CaseComplement(_) | TypeConstructor.CaseSymmetricDiff(_) => true
    case TypeConstructor.And | TypeConstructor.Or | TypeConstructor.Not => true
    case _ => false
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
