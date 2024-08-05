/*
 * Copyright 2024 Matthew Lutze
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast.TraitContext
import ca.uwaterloo.flix.language.ast.{Ast, Kind, KindedAst, RigidityEnv, SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint.Provenance
import ca.uwaterloo.flix.language.phase.typer.TypeReduction.{JavaConstructorResolutionResult, JavaMethodResolutionResult}
import ca.uwaterloo.flix.language.phase.unification.Unification.getUnderOrOverAppliedError
import ca.uwaterloo.flix.language.phase.unification._
import ca.uwaterloo.flix.util.Result.Err
import ca.uwaterloo.flix.util.collection.{ListMap, ListOps}
import ca.uwaterloo.flix.util.{InternalCompilerException, Result, Validation}

import scala.annotation.tailrec
import scala.util.chaining.scalaUtilChainingOps

object ConstraintSolver2 {

  sealed trait TypeConstraint
  object TypeConstraint {
    case class Equality(tpe1: Type, tpe2: Type) extends TypeConstraint
    case class Trait(sym: Symbol.TraitSym, tpe: Type) extends TypeConstraint
  }

  case class Tracker(private var progress: Boolean = false) extends AnyVal {
    def markProgress(): Unit = {
      progress = true
    }

    def query(): Boolean = {
      progress
    }
  }

  type ConstraintSet = List[TypeConstraint]
  type TraitEnv = Map[Symbol.TraitSym, TraitContext]

  def goAll(constrs0: ConstraintSet): (ConstraintSet, Substitution) = {
    var constrs = constrs0
    var subst = Substitution.empty
    var progressMade = true
    while (progressMade) {
      val tracker: Tracker = Tracker()
      val (newConstrs, newSubst) = goOne(constrs)(tracker)
      // invariant: the new subst is already applied to all the newConstrs
      constrs = newConstrs
      subst = newSubst @@ subst
      progressMade = tracker.query()
    }
    (constrs, subst)
  }

  def goOne(constrs: ConstraintSet)(implicit tracker: Tracker, traitEnv: TraitEnv): (ConstraintSet, Substitution) = {
    constrs
      .pipe(breakDownConstraints)
      .pipe(eliminateIdentities)
      .pipe(evaluateAliases)
      .pipe(contextReduction)
      .pipe(makeSubstitution)
  }


  // Breaks down constraints syntactically
  // (appU)
  def breakDownConstraints(constrs: ConstraintSet)(implicit tracker: Tracker): ConstraintSet = {
    def breakDownConstraint(constr: TypeConstraint): ConstraintSet = constr match {
      // TODO make sure we're looking at a syntactic type
      case TypeConstraint.Equality(Type.Apply(tpe11, tpe12, _), Type.Apply(tpe21, tpe22, _)) =>
        tracker.markProgress()
        List(TypeConstraint.Equality(tpe11, tpe21), TypeConstraint.Equality(tpe12, tpe22))

      case c: TypeConstraint => List(c)
    }

    constrs.flatMap(breakDownConstraint)
  }

  // Eliminates constraints that are the same on the left and right
  // (reflU)
  def eliminateIdentities(constrs: ConstraintSet)(implicit tracker: Tracker): ConstraintSet = {
    constrs.filter {
      case TypeConstraint.Equality(tpe1, tpe2) =>
        if (tpe1 == tpe2) {
          // Case 1: Identical types. Remove and mark progress.
          tracker.markProgress()
          false
        } else {
          // Case 2: Different types. Don't remove.
          true
        }
      case TypeConstraint.Trait(_, _) => true
    }
  }

  // Reduces trait constraints
  // (bchainE) (?)
  def contextReduction(constrs: ConstraintSet)(implicit tracker: Tracker, traitEnv: TraitEnv): ConstraintSet = ???

  // Resolve all effect constraints in the set
  // (bool or something)
  def effectUnification(constrs: ConstraintSet)(implicit tracker: Tracker): (ConstraintSet, Substitution) = ???

  // Evaluate type constraints as far as possible
  // (redU)
  def evaluateAliases(constrs: ConstraintSet)(implicit tracker: Tracker): ConstraintSet = ???

  // Build a substitution from any variable constraints
  // (varU)
  def makeSubstitution(constrs: ConstraintSet): (ConstraintSet, Substitution) = ???


}
