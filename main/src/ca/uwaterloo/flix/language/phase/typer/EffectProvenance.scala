/*
 * Copyright 2025 Gagan Chandan
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
import ca.uwaterloo.flix.language.phase.typer.EffectProvenance.Vertex.{ClockVertex, IOVertex, PureExplicitVertex, PureImplicitVertex, VarVertex}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.annotation.tailrec
import scala.collection.mutable


object EffectProvenance {


  sealed trait Vertex
  object Vertex {
    case class PureExplicitVertex(loc: SourceLocation) extends Vertex
    case class PureImplicitVertex(loc: SourceLocation) extends Vertex
    case class ClockVertex(loc: SourceLocation) extends Vertex
    case class IOVertex(loc: SourceLocation) extends Vertex
    case class VarVertex(sym: Symbol.KindedTypeVarSym) extends Vertex
  }
  type Edge = (Vertex, Vertex, SourceLocation)
  type Graph = (List[Vertex], List[Edge])

  private def toVertex(tpe: Type, provLoc: SourceLocation): List[Vertex] = tpe match {
    case Type.Var(sym, _) => List(VarVertex(sym))
    case Type.Cst(tc, loc) => tc match {
      case TypeConstructor.Pure => if (loc.isReal) List(PureExplicitVertex(loc)) else List(PureImplicitVertex(provLoc))
      case TypeConstructor.Effect(sym, _) => sym match {
        case Symbol.IO => List(IOVertex(provLoc))
        case eff => if (Symbol.mkEffSym("Clock") == eff) List(ClockVertex(provLoc)) else List()
      }
      case _ => List()
    }
    case Type.Apply(tpe1, tpe2, _) => (tpe1, tpe2) match {
      case (Type.Var(sym, _), _) => VarVertex(sym) :: toVertex(tpe2, provLoc)
      case (_, Type.Var(sym, _)) => VarVertex(sym) :: toVertex(tpe1, provLoc)
      case _ => List()
    }
    case _ => List()
  }

  def mismatch(v1: Vertex, v2: Vertex): Boolean = (v1, v2) match {
    case (_, VarVertex(_)) => false
    case (VarVertex(_), _) => false
    case (_, _) => true
  }

  private def mkError(path: Path): List[TypeConstraint] = (path.head, path.last) match {
    case (IOVertex(loc1), PureImplicitVertex(loc2)) => List(TypeConstraint.EffConflicted(TypeError.ImplicitlyPureFunctionUsesIO(loc2, loc1)))
    case (IOVertex(loc1), PureExplicitVertex(loc2)) => List(TypeConstraint.EffConflicted(TypeError.ExplicitlyPureFunctionUsesIO(loc2, loc1)))
    case _ => Nil
  }
    type Path = List[Vertex]
    sealed trait BFSColor

    object BFSColor {
      case object White extends BFSColor

      case object Grey extends BFSColor

      case object Black extends BFSColor
    }

   private type BFSVertex = (Vertex, BFSColor, Option[Vertex])
  def bfs(graph: Graph, source: Vertex): List[Path] = {
    val (vs, es) = graph
    var us: List[BFSVertex] = vs.map(v => if (v == source) (v, Grey, None) else (v, White, None))
    val s = us.find { case (v, _, _) => v == source }.getOrElse(return Nil)

    def bfsEq(v1: BFSVertex, v2: BFSVertex) = (v1, v2) match {
     case ((x, _, _), (y, _, _)) => x == y
    }

    def update(vertex: BFSVertex, vertices: List[BFSVertex]): List[BFSVertex] = {
     vertices.map {
       case v => if (bfsEq(vertex, v)) vertex else v
     }
    }

    def adj(vertex: BFSVertex, vertices: List[BFSVertex]): List[(BFSVertex)] = {
     val (v, _, _) = vertex
     es.foldLeft(List(): List[BFSVertex]) {
       case (acc, (v1, v2, _)) => if (v == v1) vertices.find{ case (u, _, _) => u == v2} match {
         case Some(res) => res :: acc
         case None => acc
       } else acc
     }
    }

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


    q.enqueue(s)

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

  def getError(constrs0: List[TypeConstraint]): List[TypeConstraint] = {
    var flow: List[Edge] = List()
    var v: Set[Vertex] = Set.empty
    constrs0.foreach {
      case TypeConstraint.Equality(tpe1, tpe2, prov) =>
        val t1 = toVertex(tpe1, prov.loc)
        val t2 = toVertex(tpe2, prov.loc)
        (t1, t2) match {
          case (Nil, _) => return Nil
          case (_, Nil) => return Nil
          case (v1::Nil, v2) =>
            prov match {
              case TypeConstraint.Provenance.ExpectEffect(_, _, _) => {
                v2.foreach(v += _)
                v = v + v1
                v2.foreach(x => flow = (x, v1, prov.loc) :: flow)
              }
              case TypeConstraint.Provenance.Source(_, _, _) => {
                v2.foreach(v += _)
                v = v + v1
                v2.foreach(x => flow = (x, v1, prov.loc) :: flow)
              }
              case TypeConstraint.Provenance.Match(_,_,_) => {
                v2.foreach(v += _)
                v = v + v1
                v2.foreach(x => flow = (x, v1, prov.loc) :: flow)
              }
              case _ => {} // TODO
            }
          case _ => {} // TODO
        }
      case _ => ()
    }
    val graph = (v.toList, flow)
    val b = v.toList.filter{
      case VarVertex(_) => false
      case _ => true
    }.flatMap(bfs(graph, _))
    b.flatMap(mkError)
  }
}
