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
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Body.{Filter, Loop}
import ca.uwaterloo.flix.language.ast.TypedAst.Predicate.Head.{False, Negative, Positive, True}
import ca.uwaterloo.flix.language.ast.TypedAst.{Root, Stratum}
import ca.uwaterloo.flix.language.errors.StratificationError
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * The stratification phase breaks constraints into strata.
  *
  * "Formally, rules are stratified if whenever there is a rule with
  * head predicate p and a negated subgoal with predicate q, there is
  * no path in the dependency graph from p to q" -- Ullman 132
  *
  * Reports a [[StratificationError]] if the constraint cannot be
  * Stratified
  */
object Stratifier extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * Returns a stratified version of the given AST `root`.
    */
  def run(root: Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = {

    val startTime = System.nanoTime()

    val stratified = stratify(root.strata, root.tables.keys.toList)
    val duration = System.nanoTime() - startTime

    for {
      stratified <- stratified
    } yield root.copy(strata = stratified).copy(time = root.time.copy(stratifier = duration))
  }

  /**
    * Stratify the graph
    */
  def stratify(stratum: List[Stratum], syms: List[Symbol.TableSym]): Validation[List[Stratum], StratificationError] = {
    // Implementing as described in Database and Knowledge - Base Systems Volume 1
    // Ullman, Algorithm 3.5 p 133

    /// Start by creating a mapping from predicates to stratum
    var strataMap = syms.map(x => (x, 1)).toMap
    val numRules = stratum.head.constraints.size

    // We repeatedly examine the rules. We move the head predicate to
    // the same strata that the body predicate is currently in if it is
    // non negated, or one after the body, if it is. We repeat until
    // there are no changes.
    //
    // If we create more strata than there are rules, then there is no
    // stratification and we error out
    var changes = true
    while(changes) {
      changes = false
      for (pred <- stratum.head.constraints) {
        pred.head match {
          case Positive(headSym, terms, loc) =>
            val currStratum = strataMap(headSym)
            for (subGoal <- pred.body) {
              subGoal match {
                case Body.Positive(subGoalSym, _, _) =>
                  val newStratum = math.max(strataMap(headSym), strataMap(subGoalSym))
                  if (newStratum > numRules) {
                    return StratificationError(stratum, headSym, subGoal, subGoalSym).toFailure
                  }
                  if (currStratum != newStratum){
                    strataMap += (headSym -> newStratum)
                    changes = true
                  }
                case Body.Negative(subGoalSym, _, _) =>
                  val newStratum = math.max(strataMap(headSym), strataMap(subGoalSym) + 1)

                  if (newStratum > numRules) {
                    return StratificationError(stratum, headSym, subGoal, subGoalSym).toFailure
                  }
                  if (currStratum != newStratum){
                    strataMap += (headSym -> newStratum)
                    changes = true
                  }
                case _:Filter => // Do Nothing
                case _:Loop => ???
              }
            }
          case _:True => // Do nothing
          case _:False => // Do nothing
          case _:Negative => ??? // Should never happen
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
      val negated = rule.body filter (x => x.isInstanceOf[Body.Negative])
      val nonNegated = rule.body filter (x => !x.isInstanceOf[Body.Negative])
      rule.copy(body = nonNegated ::: negated)
    }

    /**
      * Separate a list of strata into the levels found earlier
      */
    def separate(strata: Stratum, level: Int): List[Stratum] = strata.constraints match {
      case Nil => Stratum(Nil) :: Nil
      case lst =>
        val currStrata = lst.filter(x => (x.head match {
          case Positive(sym, _, _) => strataMap(sym)
          case True(_) =>  1
          case False(_) =>  1
          case _:Negative => ??? // Shouldn't happen
        }) == level)
        val remStrata = lst.filter(x => (x.head match {
          case Positive(sym, _, _) => strataMap(sym)
          case True(_) =>  1
          case False(_) =>  1
          case _:Negative => ??? // Shouldn't happen
        }) != level)
        Stratum(currStrata) :: separate(Stratum(remStrata), level + 1)
    }

    val reordered = Stratum(stratum.head.constraints map reorder)
    separate(reordered, 1).toSuccess
  }
}
