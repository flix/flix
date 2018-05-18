/*
 *  Copyright 2017 Magnus Madsen and Jason Mittertreiner
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Polarity
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.{Filter, Loop}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head.{Atom, False, True}
import ca.uwaterloo.flix.language.ast.TypedAst.{Root, Stratum}
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.{InternalCompilerException, Validation}
import ca.uwaterloo.flix.util.Validation._

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * Reports a [[StratificationError]] if the constraint cannot be
  * Stratified.
  *
  * This phase consists of two parts. The first computes the strata for the
  * program. If the first fails, then we continue to the second phase which
  * finds a cycle in the constraints and reports it.
  */
object Stratifier extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Stratifier") {
    val constraints = root.strata.head.constraints
    val stratified = stratify(constraints, root.tables.keys.toList)

    for {
      stratified <- stratified
    } yield root.copy(strata = stratified)
  }

  /**
    * A small case class to allow us to abstract over the possible heads in a
    * constraint
    */
  sealed trait ConstrHead
  object ConstrHead {
    case object True extends ConstrHead
    case object False extends ConstrHead
    case class Atom(sym: Symbol.TableSym) extends ConstrHead
  }

  /**
    * Create a ConstrHead type out of the head of a given constraint
    */
  def makeConstrHead(constr: TypedAst.Constraint): ConstrHead = constr.head match {
    case True(_) => ConstrHead.True
    case False(_) => ConstrHead.False
    case Atom(sym, _, _) => ConstrHead.Atom(sym)
  }


  /**
    * Stratify the graph
    */
  def stratify(constraints: List[TypedAst.Constraint], syms: List[Symbol.TableSym]): Validation[List[Stratum], StratificationError] = {
    // Implementing as described in Database and Knowledge - Base Systems Volume 1
    // Ullman, Algorithm 3.5 p 133

    /// Start by creating a mapping from predicates to stratum
    var stratumOf: Map[ConstrHead, Int] = (syms.map(x => (ConstrHead.Atom(x), 1))
      ++ List((ConstrHead.True, 1), (ConstrHead.False, 1))).toMap
    val numRules = constraints.size

    // We repeatedly examine the rules. We move the head predicate to
    // the same strata that the body predicate is currently in if it is
    // non negated, or one after the body, if it is. We repeat until
    // there are no changes.
    //
    // If we create more strata than there are rules, then there is no
    // stratification and we error out
    var changes = true
    while (changes) {
      changes = false
      for (pred <- constraints) {
        val headSym = makeConstrHead(pred)
        val currStratum = stratumOf(headSym)

        for (subGoal <- pred.body) {
          subGoal match {
            case Body.Atom(subGoalSym, Polarity.Positive, _, _) =>
              val newStratum = math.max(currStratum, stratumOf(ConstrHead.Atom(subGoalSym)))
              if (newStratum > numRules) {
                // If we create more stratum than there are rules then
                // we know there must be a negative cycle.
                return StratificationError(findNegCycle(constraints)).toFailure
              }
              if (currStratum != newStratum) {
                // Update the strata of the predicate
                stratumOf += (headSym -> newStratum)
                changes = true
              }
            case Body.Atom(subGoalSym, Polarity.Negative, _, _) =>
              val newStratum = math.max(stratumOf(headSym), stratumOf(ConstrHead.Atom(subGoalSym)) + 1)

              if (newStratum > numRules) {
                // If we create more stratum than there are rules then
                // we know there must be a negative cycle
                return StratificationError(findNegCycle(constraints)).toFailure
              }
              if (currStratum != newStratum) {
                // Update the strata of the predicate
                stratumOf += (headSym -> newStratum)
                changes = true
              }
            case _: Filter => // Do Nothing
            case _: Loop => // Do Nothing
          }
        }
      }
    }

    // We now have a mapping from predicates to strata, apply it to the
    // list of rules we are given.
    //
    // While we're at it, we also reorder the goals so that negated
    // literals occur after non negated ones. This ensures that the
    // variables in the negated literals will be bound when we evaluate
    // them

    /**
      * Reorder a rule so that negated literals occur last
      */
    def reorder(rule: TypedAst.Constraint): TypedAst.Constraint = {
      def isNegative(p: Body): Boolean = p match {
        case Body.Atom(_ , Polarity.Negative, _, _) => true
        case _ => false
      }

      val negated = rule.body filter isNegative
      val nonNegated = rule.body filter (x => !isNegative(x))
      rule.copy(body = nonNegated ::: negated)
    }

    /**
      * Separate a list of strata into the levels found earlier
      */
    def separate(constraints: List[TypedAst.Constraint], currLevel: Int): List[List[TypedAst.Constraint]] = {
      // We consider the True and False atoms to be in the first stratum
      def getLevel(c: TypedAst.Constraint): Int = stratumOf(makeConstrHead(c))

      constraints match {
        case Nil => Nil
        case lst =>
          val currStrata = lst.filter(x => getLevel(x) == currLevel)
          val remStrata = lst.filter(x => getLevel(x) != currLevel)
          currStrata :: separate(remStrata, currLevel + 1)
      }
    }

    val separated = separate(constraints, 1)
    val reordered = separated map {
      _ map reorder
    }
    val strata = reordered.map(Stratum) match {
      case Nil => List(Stratum(Nil))
      case lst => lst
    }
    strata.toSuccess
  }


  /**
    * An edge represents a constraint in the program. It has a weight of -1
    * if it is a negated predicate, else it has a weight of 0
    */
  case class Edge(weight: Int, rule: TypedAst.Constraint)

  /**
    * A graph is an adjacency list of Constraint -> (Constraint, Weight)
    * where weight is either -1 or 0
    */
  class Graph {

    var vertices: Set[Symbol.TableSym] = Set.empty

    // An adjacency list of from - to edges with weights
    var edges: Map[Symbol.TableSym, Map[Symbol.TableSym, Edge]] = Map.empty

    /**
      * Insert an edge from-to with weight into the graph
      */
    def insert(from: Symbol.TableSym, to: Symbol.TableSym, constr: TypedAst.Constraint, weight: Int): Stratifier.Graph = {
      edges = edges.get(from) match {
        case Some(m) => edges + (from -> (m + (to -> Edge(weight, constr))))
        case None => edges + (from -> Map(to -> Edge(weight, constr)))
      }
      vertices += from
      vertices += to
      this
    }
  }

  /**
    * Use the constraints to create an adjacency matrix
    *
    * A constraint of:
    * P :- Q creates an edge Q -> P of weight 0
    * P :- !Q creates an edge Q -> P of weight -1
    */
  def createGraph(constraints: List[TypedAst.Constraint]): Graph = {
    constraints.foldRight(new Graph)((constraint: TypedAst.Constraint, graph: Graph) => constraint.head match {
      case TypedAst.Predicate.Head.Atom(headSym, _, _) =>
        // As well as creating the graph out of the given constraints, we also add a source node which
        // has a directed edge to all nodes
        graph.insert(null, headSym, constraint, 0)
        constraint.body.foldRight(graph)((pred: TypedAst.Predicate.Body, graph: Graph) => pred match {
          case TypedAst.Predicate.Body.Atom(predSym, Polarity.Positive, _, _) =>
            graph.insert(headSym, predSym, constraint, 0)
          case TypedAst.Predicate.Body.Atom(predSym, Polarity.Negative, _, _) =>
            graph.insert(headSym, predSym, constraint, -1)
          case _: TypedAst.Predicate.Body.Filter => graph
          case _: TypedAst.Predicate.Body.Loop => graph
        })
      case TypedAst.Predicate.Head.True(_) => graph
      case TypedAst.Predicate.Head.False(_) => graph
    })
  }

  /**
    * Returns true iff a graph has a negative cycle
    */
  def findNegCycle(constraints: List[TypedAst.Constraint]): List[TypedAst.Constraint] = {
    // Get the list of constraints
    val g = createGraph(constraints)

    // The Bellman Ford algorithm, we can use it to find negative cycles

    // Initialize the distance and predecessor arrays to default values
    var distPred: Map[Symbol.TableSym, (Int, TypedAst.Constraint)] = g.vertices.toList.zip(List.fill(g.vertices.size)((1, null))).toMap
    // The source has 0 weight
    distPred = distPred.updated(null, (0, null))

    // Relax the vertices |V|+1 times. The path can't be longer than |V|
    for (_ <- 0 to g.vertices.size + 1) {
      for (fromEntry <- g.edges) {
        for (toEntry <- fromEntry._2) {
          val from = fromEntry._1
          val to = toEntry._1
          val weight = toEntry._2.weight
          // Relax the edge from-to
          if (distPred(from)._1 + weight < distPred(to)._1) {
            distPred = distPred.updated(to, (distPred(from)._1 + weight, toEntry._2.rule))
          }
        }
      }
    }

    // At this point, we must have found the shortest path, so if there is a
    // shorter path, then there is a negative cycle, and if so, we return it
    var witness: List[TypedAst.Constraint] = Nil
    // Look for an edge (u,v) where distance(u) + Weight(u,v) < distance(v)
    for (fromEntry <- g.edges) {
      for (toEntry <- fromEntry._2) {
        val from = fromEntry._1
        val to = toEntry._1
        val weight = toEntry._2.weight
        if (distPred(from)._1 + weight < distPred(to)._1) {
          // We found part of a negative cycle
          // Go backwards from v until we find a cycle
          val firstEdge = distPred(from)._2
          witness = firstEdge :: witness

          var fromConstraint = distPred(firstEdge.head match {
            case Atom(sym, _, _) => sym
            case _: False => throw InternalCompilerException("Encountered the False atom while looking for negative cyles which should never happen")
            case _: True => throw InternalCompilerException("Encountered the True atom while looking for negative cyles which should never happen")
          })._2

          while (fromConstraint != null && fromConstraint != firstEdge) {
            witness = fromConstraint :: witness
            fromConstraint = distPred(fromConstraint.head match {
              case Atom(sym, _, _) => sym
              case _: False => throw InternalCompilerException("Encountered the False atom while looking for negative cyles which should never happen")
              case _: True => throw InternalCompilerException("Encountered the True atom while looking for negative cyles which should never happen")
            })._2
          }
          return witness
        }
      }
    }
    witness
  }
}
