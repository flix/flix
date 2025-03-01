/*
 * Copyright 2024 Magnus Madsen
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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.language.ast.shared.TraitConstraint
import ca.uwaterloo.flix.language.ast.{KindedAst, RigidityEnv}
import ca.uwaterloo.flix.language.phase.typer.InfResult
import ca.uwaterloo.flix.language.phase.unification.set.Equation
import ca.uwaterloo.flix.language.phase.unification.{EqualityEnv, TraitEnv}

/**
  * A common super-type for Flix events.
  */
sealed trait FlixEvent

object FlixEvent {

  /**
    * An event that is fired when new type constraints are collected for the given def symbol `sym`.
    */
  case class NewConstraintsDef(defn: KindedAst.Def, infResult: InfResult, renv: RigidityEnv, tconstrs: List[TraitConstraint], tenv: TraitEnv, eqEnv: EqualityEnv, root: KindedAst.Root) extends FlixEvent

  /**
    * An event that is fired when a new system of Boolean equation is about to be solved.
    */
  case class SolveEffEquations(econstrs: List[Equation]) extends FlixEvent

}
