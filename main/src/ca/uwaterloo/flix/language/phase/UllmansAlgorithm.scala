/*
 * Copyright 2021 Jonathan Lindegaard Starup
 * Copyright 2021 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, Name, SourceLocation, Type}
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Validation.{ToFailure, ToSoftFailure, ToSuccess}
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}

import java.util.Objects
import scala.annotation.tailrec
import scala.collection.mutable

object UllmansAlgorithm {

  /**
    * Represents a dependency between two predicate symbols.
    */
  sealed trait DependencyEdge {
    def head: Name.Pred

    def body: Name.Pred

    override def hashCode(): Int = this match {
      case _: DependencyEdge.Weak => 5 * Objects.hash(head, body)
      case _: DependencyEdge.Strong => 7 * Objects.hash(head, body)
    }

    override def equals(that: Any): Boolean = (this, that) match {
      case (DependencyEdge.Weak(head1, body1, _), DependencyEdge.Weak(head2, body2, _)) =>
        head1 == head2 && body1 == body2
      case (DependencyEdge.Strong(head1, body1, _), DependencyEdge.Strong(head2, body2, _)) =>
        head1 == head2 && body1 == body2
      case _ => false
    }
  }

  object DependencyEdge {

    /**
      * Represents an edge between `body` and `head` which means that the strata
      * of `head` must be higher than or equal to the strata of `body`.
      */
    case class Weak(head: Name.Pred, body: Name.Pred, loc: SourceLocation) extends DependencyEdge

    /**
      * Represents an edge between `body` and `head` which means that the strata
      * of `head` must be strictly higher than the strata of `body`.
      */
    case class Strong(head: Name.Pred, body: Name.Pred, loc: SourceLocation) extends DependencyEdge
  }

  /**
    * Represents the separated dependency graph oblivious to the rules that contain them.
    */
  type DependencyGraph = Set[DependencyEdge]

  /**
    * Computes the stratification of the given dependency graph `g` at the given source location `loc`.
    *
    * See Database and Knowledge - Base Systems Volume 1 Ullman, Algorithm 3.5 p 133
    */
  def stratify(g: DependencyGraph, tpe: Type, loc: SourceLocation)(implicit flix: Flix): Validation[Ast.Stratification, StratificationError] = {
    //
    // Maintain a mutable map from predicates to their (maximum) stratum number.
    //
    // Any predicate not explicitly in the map has a default value of zero.
    //
    val stratumOf = mutable.Map.empty[Name.Pred, Int]

    //
    // Compute the number of dependency edges.
    //
    // The number of strata is bounded by the number of predicates which is bounded by the number of edges.
    //
    // Hence if we ever compute a stratum higher than this number then there is a strict cycle, i.e a cycle
    // with at least one strong edge.
    //
    val maxStratum = g.size

    //
    // Repeatedly examine the dependency edges.
    //
    // We always consider two cases:
    //   1. A weak body predicate requires its head predicate to be in its stratum or any higher stratum.
    //   2. A strong body predicate requires its head predicate to be in a strictly higher stratum.
    //
    // If we ever create more strata than there are dependency edges then there is a strong cycle and we abort.
    //
    var changed = true
    while (changed) {
      changed = false

      // Examine each dependency edge in turn.
      for (edge <- g) {
        edge match {
          case DependencyEdge.Weak(headSym, bodySym, _) =>
            // Case 1: The stratum of the head must be in the same or a higher stratum as the body.
            val headStratum = stratumOf.getOrElseUpdate(headSym, 0)
            val bodyStratum = stratumOf.getOrElseUpdate(bodySym, 0)

            if (!(headStratum >= bodyStratum)) {
              // Put the head in the same stratum as the body.
              stratumOf.put(headSym, bodyStratum)
              changed = true
            }

          case currentEdge@DependencyEdge.Strong(headSym, bodySym, _) =>
            // Case 2: The stratum of the head must be in a strictly higher stratum than the body.
            val headStratum = stratumOf.getOrElseUpdate(headSym, 0)
            val bodyStratum = stratumOf.getOrElseUpdate(bodySym, 0)

            if (!(headStratum > bodyStratum)) {
              // Put the head in one stratum above the body stratum.
              val newHeadStratum = bodyStratum + 1
              stratumOf.put(headSym, newHeadStratum)
              changed = true

              // Check if we have found a strong cycle.
              if (newHeadStratum > maxStratum) {
                return StratificationError(findStrongCycle(currentEdge, g), tpe, loc).toFailure
              }
            }
        }
      }
    }

    // We are done. Successfully return the computed stratification.
    Ast.Stratification(stratumOf.toMap).toSuccess
  }

  /**
    * Returns a path that forms a strong cycle, initially attempting to find
    * a cycle around the `firstCheck` edge.
    */
  private def findStrongCycle(firstCheck: DependencyEdge.Strong, g: DependencyGraph): List[(Name.Pred, SourceLocation)] = {
    // Computes a map from predicates to their successors.
    val succ = mutable.Map.empty[Name.Pred, Set[(Name.Pred, SourceLocation)]]
    for (edge <- g) {
      edge match {
        case DependencyEdge.Weak(head, body, loc) =>
          val s = succ.getOrElse(body, Set.empty)
          succ.put(body, s + ((head, loc)))
        case DependencyEdge.Strong(head, body, loc) =>
          val s = succ.getOrElse(body, Set.empty)
          succ.put(body, s + ((head, loc)))
      }
    }

    // Find one of the shortest paths from `start` to `goal`
    def bfs(src: Name.Pred, dst: Name.Pred): Option[mutable.Map[Name.Pred, (Name.Pred, SourceLocation)]] = {
      // A map from predicates to one of their immediate
      // predecessors in the BFS.
      val pred = mutable.Map.empty[Name.Pred, (Name.Pred, SourceLocation)]
      // A set of previously seen predicates.
      val seen = mutable.Set.empty[Name.Pred]
      // the tasklist for bfs
      val tasklist = mutable.Queue.empty[Name.Pred]
      seen.add(src)
      tasklist.enqueue(src)

      while (tasklist.nonEmpty) {
        val current = tasklist.dequeue()
        if (current == dst) return Some(pred)
        succ.getOrElse(current, Set.empty).foreach {
          case (next, ruleLoc) =>
            if (!seen.contains(next)) {
              pred.update(next, (current, ruleLoc))
              seen.add(next)
              tasklist.enqueue(next)
            }
        }
      }
      None
    }

    // Recursively constructs a path from `endPoint` and backwards through the graph.
    def unroll(from: Name.Pred, to: Name.Pred, pred: mutable.Map[Name.Pred, (Name.Pred, SourceLocation)]): List[(Name.Pred, SourceLocation)] = {
      @tailrec
      def unrollHelper(s: Name.Pred, acc: List[(Name.Pred, SourceLocation)]): List[(Name.Pred, SourceLocation)] = {
        if (s == to) return acc
        pred.getOrElse(s, throw InternalCompilerException("Stratification cycle malformed", s.loc)) match {
          case (prev, loc) => unrollHelper(prev, (prev, loc) :: acc)
        }
      }

      unrollHelper(from, Nil).reverse
    }

    // For each strong edge, `x -> y`, in `edges` it is checked if `y` can
    // reach `x`. The first such cycle is returned. If none are found, an
    // `InternalCompilerException` is thrown.
    @tailrec
    def checkCycles(edges: List[DependencyEdge]): List[(Name.Pred, SourceLocation)] = edges match {
      case edge :: next => edge match {
        case DependencyEdge.Weak(_, _, _) => checkCycles(next)
        case DependencyEdge.Strong(head, body, loc) =>
          bfs(head, body) match {
            case None => checkCycles(next)
            case Some(pred) =>
              // we found a cycle and can report it
              (body, loc) :: unroll(body, head, pred) ::: (body, loc) :: Nil
          }
      }
      case Nil => throw InternalCompilerException("Stratification error without a strong cycle", SourceLocation.Unknown)
    }

    // We do not know where the cycle is but often it includes the edge
    // `firstCheck` since that edge brought ullman over the limit so
    // we try to find that cycle first and then check every other strong edge.
    checkCycles(firstCheck :: g.toList)
  }

}
