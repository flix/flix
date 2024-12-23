/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.*

/** Generates [[Equation]] based on existing [[SetFormula]]. */
object EquationGenerator {

  /** Returns the solvable [[Equation]] `f xor f ~ empty`. */
  def xorSelfEmpty(f: SetFormula): Equation = mkEq(mkXor(f, f), Empty)

  /** Returns the solvable [[Equation]] `(f - aug) xor (f ∪ aug) ~ aug `. */
  def xorSelfAugmented(f: SetFormula, augment: SetFormula): Equation = {
    mkEq(mkXor(mkDifference(f, augment), mkUnion(f, augment)), augment)
  }

  /** Returns an [[Equation]] with [[SourceLocation.Unknown]]. */
  def mkEq(f1: SetFormula, f2: SetFormula): Equation = Equation.mk(f1, f2, SourceLocation.Unknown)

}
