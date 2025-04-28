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

import ca.uwaterloo.flix.language.ast.shared.Source
import ca.uwaterloo.flix.language.ast.{ReducedAst, Symbol, Token}
import ca.uwaterloo.flix.language.phase.typer.TypeConstraint
import ca.uwaterloo.flix.language.phase.unification.set.Equation

/**
  * A common super-type for Flix events.
  */
sealed trait FlixEvent

object FlixEvent {

  case class AfterLexer(sources: Map[Source, Array[Token]]) extends FlixEvent

  /**
    * An event that is fired after the tailpos phase.
    */
  case class AfterTailPos(root: ReducedAst.Root) extends FlixEvent

  /**
    * An event that is fired when new type constraints are collected for the given def symbol `sym`.
    */
  case class NewConstraintsDef(sym: Symbol.DefnSym, tconstrs: List[TypeConstraint]) extends FlixEvent

  /**
   * An event that is fired when a new system of Boolean equation is about to be solved.
   */
  case class SolveEffEquations(econstrs: List[Equation]) extends FlixEvent

}
