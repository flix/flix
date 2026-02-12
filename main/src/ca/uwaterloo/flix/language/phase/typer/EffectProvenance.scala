/*
 * Copyright 2026 Alexander Sommer, Samuel Skovbakke
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
package ca.uwaterloo.flix.language.phase.typer

import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.phase.typer.EffectProvenance.BFSColor.{Black, Grey, White}
import ca.uwaterloo.flix.language.phase.typer.EffectProvenance.Vertex.{CstVertex, IOVertex, PureExplicitVertex, PureImplicitVertex, VarVertex}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.EffConflicted

import scala.annotation.tailrec
import scala.collection.mutable

/**
  * Analyzes effect provenance to detect conflicts of pure functions using effects.
  *
  * This module constructs a directed graph representing effect flow from type constraints
  * and performs breadth-first search to identify invalid effect paths. An invalid path
  * exists when an effect (IO or other effect) flows to a pure context (explicit or implicit).
  */
object EffectProvenance {

  /**
    * Color used in BFS search traversal.
    */
  sealed trait BFSColor
  object BFSColor {
    /** Unvisited vertex. */
    case object White extends BFSColor

    /** Vertex in the queue. */
    case object Grey extends BFSColor

    /** Fully processed vertex. */
    case object Black extends BFSColor
  }

  /**
    * Represents a vertex in the BFS with its color and parent information.
    */
  private type BFSVertex = (Vertex, BFSColor, Option[Vertex])

  /**
    * Represents a directed edge in the effect graph.
    */
  private type Edge = (Vertex, Vertex, SourceLocation)

  /**
    * Represents a directed graph of effect flow.
    */
  private type Graph = (List[Vertex], List[Edge])

  /**
    * Represents a path through the effect graph.
    */
  private type Path = List[Vertex]

  /**
    * Represents a vertex in the effect graph.
    *
    * A Vertex can represent:
    *   - A Pure contexts (explicit or implicit)
    *   - An IO effect
    *   - A non-reserved effect constant
    *   - An effect variable
    */
  sealed trait Vertex
  object Vertex {
    /**
      * An explicitly pure context (has a real source location).
      *
      * @param loc the source location of {}
      */
    case class PureExplicitVertex(loc: SourceLocation) extends Vertex

    /**
      * An implicitly pure context.
      *
      * @param loc the synthetic location, which is the position immediately to the right of the return type.
      */
    case class PureImplicitVertex(loc: SourceLocation) extends Vertex

    /**
      * A non-reserved effect constant.
      *
      * @param sym the effect symbol
      * @param loc the source location where the effect is used
      */
    case class CstVertex(sym: Symbol.EffSym, loc: SourceLocation) extends Vertex

    /**
      * The IO effect.
      *
      * @param loc the source location where IO is used
      */
    case class IOVertex(loc: SourceLocation) extends Vertex

    /**
      * An effect variable.
      *
      * @param sym the effect variable symbol
      */
    case class VarVertex(sym: Symbol.KindedTypeVarSym) extends Vertex
  }


  /**
    * Analyzes type constraints for effect conflicts.
    *
    * Returns a list of effect conflicts if any are found, or None if no good effect error could be made.
    * This is the main entry point for effect provenance analysis.
    *
    * @param constrs0 the list of type constraints to analyze
    * @return Some(conflicts) if conflicts could be found, None otherwise
    */
  def getError(constrs0: List[TypeConstraint]): Option[List[EffConflicted]] = {
    val graph = buildGraph(constrs0)
    graph.flatMap(findErrorsInGraph)
  }

  /**
    * Performs breadth-first search from a source vertex to find all invalid paths.
    *
    * A path is invalid if it leads from the source to a conflicting vertex.
    * For example, a path from IOVertex to PureExplicitVertex is invalid.
    *
    * @param graph  the effect graph (vertices, edges)
    * @param source the vertex to start BFS from
    * @return all invalid paths found from the source
    */
  private def bfs(graph: Graph, source: Vertex): List[Path] = {
    val (vs, es) = graph
    var us: List[BFSVertex] = vs.map(v => if (v == source) (v, Grey, None) else (v, White, None))
    val start = us.find { case (v, _, _) => v == source }.getOrElse(return Nil)

    /**
      * Checks if two BFS vertices are equal.
      */
    def bfsEq(v1: BFSVertex, v2: BFSVertex) = (v1, v2) match {
      case ((x, _, _), (y, _, _)) => x == y
    }

    /**
      * Updates a vertex in the BFS vertex list.
      */
    def update(vertex: BFSVertex, vertices: List[BFSVertex]): List[BFSVertex] = {
      vertices.map {
        case v => if (bfsEq(vertex, v)) vertex else v
      }
    }

    /**
      * Gets all vertices adjacent to the given vertex.
      */
    def adj(vertex: BFSVertex, vertices: List[BFSVertex]): List[(BFSVertex)] = {
      val (v, _, _) = vertex
      es.foldLeft(List(): List[BFSVertex]) {
        case (acc, (v1, v2, _)) => if (v == v1) vertices.find{ case (u, _, _) => u == v2} match {
          case Some(res) => res :: acc
          case None => acc
        } else acc
      }
    }

    /**
      * Constructs a list of reachable vertices from a starting vertex.
      */
    def reachable(start: BFSVertex, bfs: List[BFSVertex]): List[Vertex] = {
      @tailrec
      def helper(vertex1: BFSVertex, subList: List[BFSVertex], acc: List[Vertex]): List[Vertex] = subList match {
        case (next@(v, _, _)) :: xs => vertex1._3 match {
          case Some(parent) => if (v == parent) helper(next, bfs, v :: acc) else helper(vertex1, xs, acc)
          case None => acc
        }
        case Nil => acc
      }
      helper(start, bfs, List(start._1))
    }

    val q: mutable.Queue[BFSVertex] = mutable.Queue.empty


    q.enqueue(start)

    while (q.nonEmpty) {
      val u = q.dequeue()
      adj(u, us).foreach {
        case (v, White, _) =>
          val v1 = (v, Grey, Some(u._1))
          us = update(v1, us)
          q.enqueue(v1)
        case (_, _, _) => {}
      }
      val u1 = (u._1, Black, u._3)
      us = update(u1, us)
    }


    us.map(v => {
      val r = reachable(v, us)
      if (r.tail.forall(!mismatch(r.head, _))) Nil else r
    }).filterNot(_.isEmpty)
  }

  /**
    * Constructs an effect provenance graph from type constraints.
    *
    * The graph represents flow of effects through the program. An edge from A to B
    * means that effect A flows to B.
    *
    * @param constraints the type constraints to analyze
    * @return Some(graph) if construction succeeds, None if an edge could not be created.
    */
  private def buildGraph(constraints: List[TypeConstraint]): Option[Graph] = {
    var flow: List[Edge] = List()
    var v: Set[Vertex] = Set.empty
    constraints.foreach {
      case TypeConstraint.Equality(tpe1, tpe2, prov) =>
        val vtpe1 = toVertex(tpe1, prov.loc)
        val vtpe2 = toVertex(tpe2, prov.loc)
        (vtpe1, vtpe2) match {
          case (Nil, _) => return None
          case (_, Nil) => return None
          case (to::Nil, fromVertices) =>
            fromVertices.foreach(v += _)
            v = v + to
            prov match {
              case TypeConstraint.Provenance.ExpectEffect(_, _, _) => {
                fromVertices.foreach(from => flow = (from, to, prov.loc) :: flow)
              }
              case TypeConstraint.Provenance.Source(_, _, _) => {
                fromVertices.foreach(from => flow = (from, to, prov.loc) :: flow)
              }
              case TypeConstraint.Provenance.Match(_,_,_) => {
                fromVertices.foreach(from => flow = (from, to, prov.loc) :: flow)
              }
              case _ => {}
            }
          case _ => {}
        }
      case _ => ()
    }
    Some((v.toList, flow))
  }

  /**
    * Finds all effect conflicts in the graph.
    *
    * A conflict occurs when there is a path from an impure effect (IO or custom effect)
    * to a pure context. This is detected by running BFS from each effect vertex.
    *
    * @param graph the effect graph
    * @return Some(conflicts) if conflicts could be found, None otherwise
    */
  private def findErrorsInGraph(graph: Graph): Option[List[EffConflicted]] = {
    val (v, _) = graph
    val errs = v.filter{
      case VarVertex(_) => false
      case _ => true
    }.flatMap(bfs(graph, _)).flatMap(mkError)
    errs match {
      case Nil => None
      case l => Some(l)
    }
  }

  /**
    * Checks if two vertices represent a conflict.
    *
    * Effect variables never conflict with anything since they can unify.
    * All concrete vertices conflict with each other.
    *
    * @param v1 the first vertex
    * @param v2 the second vertex
    * @return true if the vertices represent a conflict, false otherwise
    */
  private def mismatch(v1: Vertex, v2: Vertex): Boolean = (v1, v2) match {
    case (_, VarVertex(_)) => false
    case (VarVertex(_), _) => false
    case (_, _) => true
  }

  /**
    * Converts a path to an error if there is a conflict.
    *
    * A conflict occurs when the path starts at an impure vertex and ends at a pure vertex.
    *
    * @param path the path to analyze
    * @return a list containing the error (if any)
    */
  private def mkError(path: Path): List[EffConflicted] = (path.head, path.last) match {
    case (IOVertex(loc1), PureImplicitVertex(loc2)) => List(TypeConstraint.EffConflicted(TypeError.ImplicitlyPureFunctionUsesIO(loc2, loc1)))
    case (IOVertex(loc1), PureExplicitVertex(loc2)) => List(TypeConstraint.EffConflicted(TypeError.ExplicitlyPureFunctionUsesIO(loc2, loc1)))
    case (CstVertex(sym, loc1) , PureExplicitVertex(loc2)) => List(TypeConstraint.EffConflicted(TypeError.ExplicitlyPureFunctionUsesEffect(sym, loc2, loc1)))
    case (CstVertex(sym, loc1) , PureImplicitVertex(loc2)) => List(TypeConstraint.EffConflicted(TypeError.ImplicitlyPureFunctionUsesEffect(sym, loc2, loc1)))
    case _ => Nil
  }

  /**
    * Converts a type to its vertex representation(s).
    *
    * A type may map to multiple vertices (e.g., a union type), a single vertex,
    * or no vertices (for non-effect types). Returns an empty list if the type
    * cannot be represented as vertices.
    *
    * @param tpe the type to convert
    * @param constLoc the constraint location from where the type was encountered
    * @return the list of vertices representing this type
    */
  private def toVertex(tpe: Type, constLoc: SourceLocation): List[Vertex] = tpe match {
    case Type.Var(sym, _) => List(VarVertex(sym))
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Pure => if (loc.isReal) List(PureExplicitVertex(loc)) else List(PureImplicitVertex(constLoc))
      case TypeConstructor.Effect(sym, _) => sym match {
        case Symbol.IO => List(IOVertex(constLoc))
        case Symbol.Debug => Nil
        case eff => List(CstVertex(eff, constLoc))
      }
      case _ => List()
    }
    case Type.Apply(tpe1, tpe2, _) => (tpe1, tpe2) match {
      case (Type.Var(sym, _), _) => VarVertex(sym) :: toVertex(tpe2, constLoc)
      case (_, Type.Var(sym, _)) => VarVertex(sym) :: toVertex(tpe1, constLoc)
      case (Type.Apply(_, _, _), _) => toVertex(tpe1, constLoc) ::: toVertex(tpe2, constLoc)
      case (_, Type.Apply(_,_,_)) => toVertex(tpe1, constLoc) ::: toVertex(tpe2, constLoc)
      case _ => List()
    }
    case _ => List()
  }
}
