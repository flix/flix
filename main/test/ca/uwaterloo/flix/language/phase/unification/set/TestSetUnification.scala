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

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.phase.typer.Progress
import ca.uwaterloo.flix.language.phase.unification.set.EquationGenerator.mkEq
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.{Cst, Empty, Union, Var}
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification.SolverListener
import ca.uwaterloo.flix.util.TwoList
import org.scalatest.funsuite.AnyFunSuite

class TestSetUnification extends AnyFunSuite with TestUtils {

  implicit val opts: SetUnification.Options = SetUnification.Options.default
  implicit val listener: SolverListener = SolverListener.DoNothing
  implicit val progress: Progress = Progress()

  test("Iterator.mapWithIndex") {
    val input = List(
      mkEq(Var(0), Cst(21)),
      mkEq(Var(9), Var(10)),
      mkEq(Var(10), Var(0)),
      mkEq(Var(10), Var(11)),
      mkEq(Var(11), Var(1)),
      mkEq(Var(2), Empty),
      mkEq(Var(10), Var(12)),
      mkEq(Var(3), Union(TwoList(Var(12), Var(2), List()))),
      mkEq(Var(14), Empty),
      mkEq(Var(14), Var(4)),
      mkEq(Var(13), Cst(19)),
      mkEq(Var(5), Union(TwoList(Var(13), Var(4), List()))),
      mkEq(Var(15), Union(TwoList(Var(9), Var(8), List()))),
      mkEq(Var(15), Var(6)),
      mkEq(Var(16), Union(TwoList(Var(1), Var(3), List(Var(5))))),
      mkEq(Var(16), Var(7)),
      mkEq(Var(9), Var(18)),
      mkEq(Union(TwoList(Var(18), Var(17), List())), Union(TwoList(Var(6), Var(7), List()))),
      mkEq(Var(17), Union(TwoList(Cst(20), Cst(19), List()))),
      mkEq(Var(18), Cst(21)),
      mkEq(Var(8), Cst(20)),
      mkEq(Var(9), Cst(21))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

}
