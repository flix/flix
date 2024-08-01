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

object ConstraintSolver2 {

  type ConstraintSet = Nothing

  case class Res(rest: ConstraintSet, failed: ConstraintSet)


  // Breaks down constraints syntactically
  // (appU)
  def breakDownConstraint(constrs: ConstraintSet): Res = ???

  // Eliminates constraints that are the same on the left and right
  // (reflU)
  def eliminateIdentities(constrs: ConstraintSet): Res = ???

  // Reduces trait constraints
  // (bchainE) (?)
  def contextReduction(constrs: ConstraintSet): Res = ???

  // Resolve all effect constraints in the set
  // (bool or something)
  def effectUnification(constrs: ConstraintSet): Res = ???

  // Evaluate type constraints as far as possible
  // (redU)
  def evaluateAliases(constrs: ConstraintSet): Res = ???

  // Build a substitution from any variable constraints
  // (varU)
  def makeSubstitution(constrs: ConstraintSet): (Res, Substitution) = ???


}
