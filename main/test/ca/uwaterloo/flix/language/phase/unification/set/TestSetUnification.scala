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
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.{Cst, Empty, Union, Var}
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification.SolverListener
import ca.uwaterloo.flix.util.collection.TwoList
import org.scalatest.funsuite.AnyFunSuite

class TestSetUnification extends AnyFunSuite with TestUtils {

  implicit val listener: SolverListener = SolverListener.DoNothing

  def mkEq(f1: SetFormula, f2: SetFormula): Equation = Equation.mk(f1, f2, SourceLocation.Unknown)

  test("Array.scanLeft") {
    val input = List(
      mkEq(Union(TwoList(Cst(21), Cst(20), List())), Union(TwoList(Var(1), Var(2), List(Var(11))))),
      mkEq(Var(12), Cst(20)),
      mkEq(Var(0), Empty),
      mkEq(Var(0), Var(1)),
      mkEq(Var(13), Cst(20)),
      mkEq(Var(13), Var(2)),
      mkEq(Var(13), Cst(20)),
      mkEq(Var(3), Empty),
      mkEq(Var(15), Empty),
      mkEq(Var(15), Var(4)),
      mkEq(Var(5), Empty),
      mkEq(Var(16), Cst(20)),
      mkEq(Var(6), Union(TwoList(Var(16), Var(5), List()))),
      mkEq(Var(14), Cst(21)),
      mkEq(Var(7), Union(TwoList(Var(14), Var(4), List(Var(6))))),
      mkEq(Var(17), Cst(20)),
      mkEq(Var(17), Var(8)),
      mkEq(Var(9), Empty),
      mkEq(Var(10), Union(TwoList(Var(18), Var(9), List()))),
      mkEq(Var(18), Union(TwoList(Var(3), Var(7), List(Var(8), Var(10))))),
      mkEq(Var(19), Var(18)),
      mkEq(Var(19), Var(11))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

  test("Array.unzip") {
    val input = List(
      mkEq(Union(TwoList(Cst(25), Cst(26), List(Cst(27)))), Union(TwoList(Var(0), Var(1), List(Var(2), Var(3), Var(4), Var(5), Var(6), Var(13))))),
      mkEq(Var(14), Cst(27)),
      mkEq(Var(0), Empty),
      mkEq(Var(1), Empty),
      mkEq(Var(15), Cst(25)),
      mkEq(Var(15), Var(2)),
      mkEq(Var(16), Cst(26)),
      mkEq(Var(16), Var(3)),
      mkEq(Var(17), Cst(27)),
      mkEq(Var(17), Var(4)),
      mkEq(Var(18), Cst(25)),
      mkEq(Var(18), Var(5)),
      mkEq(Var(18), Cst(25)),
      mkEq(Var(19), Cst(26)),
      mkEq(Var(19), Var(6)),
      mkEq(Var(19), Cst(26)),
      mkEq(Var(7), Empty),
      mkEq(Var(20), Cst(27)),
      mkEq(Var(20), Var(8)),
      mkEq(Var(21), Cst(25)),
      mkEq(Var(21), Var(9)),
      mkEq(Var(22), Cst(26)),
      mkEq(Var(22), Var(10)),
      mkEq(Var(11), Empty),
      mkEq(Var(12), Union(TwoList(Var(23), Var(11), List()))),
      mkEq(Var(23), Union(TwoList(Var(7), Var(8), List(Var(9), Var(10), Var(12))))),
      mkEq(Var(24), Var(23)),
      mkEq(Var(24), Var(13)),
      mkEq(Var(15), Cst(25)),
      mkEq(Var(16), Cst(26))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

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

  test("MutDeque.toArray") {
    val input = List(
      mkEq(Union(TwoList(Cst(30), Cst(29), List())), Union(TwoList(Var(0), Var(1), List(Var(2), Var(3), Var(4), Var(5), Var(7), Var(12))))),
      mkEq(Var(13), Cst(30)),
      mkEq(Var(13), Var(0)),
      mkEq(Var(14), Cst(30)),
      mkEq(Var(1), Var(14)),
      mkEq(Var(15), Cst(30)),
      mkEq(Var(2), Var(15)),
      mkEq(Var(3), Empty),
      mkEq(Var(16), Cst(29)),
      mkEq(Var(16), Var(4)),
      mkEq(Var(5), Empty),
      mkEq(Var(19), Cst(30)),
      mkEq(Var(6), Var(19)),
      mkEq(Var(17), Cst(29)),
      mkEq(Var(19), Var(18)),
      mkEq(Var(7), Union(TwoList(Var(18), Var(17), List(Var(6))))),
      mkEq(Var(25), Cst(30)),
      mkEq(Var(8), Var(25)),
      mkEq(Var(23), Cst(29)),
      mkEq(Var(25), Var(24)),
      mkEq(Var(9), Union(TwoList(Var(24), Var(23), List(Var(8))))),
      mkEq(Var(28), Cst(30)),
      mkEq(Var(10), Var(28)),
      mkEq(Var(26), Cst(29)),
      mkEq(Var(28), Var(27)),
      mkEq(Var(11), Union(TwoList(Var(27), Var(26), List(Var(10))))),
      mkEq(Var(20), Cst(29)),
      mkEq(Var(23), Var(21)),
      mkEq(Var(26), Var(22)),
      mkEq(Var(20), Cst(29)),
      mkEq(Var(12), Union(TwoList(Var(21), Var(22), List(Var(20), Var(9), Var(11))))),
      mkEq(Var(17), Cst(29)),
      mkEq(Var(16), Cst(29))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

  test("MutList.map") {
    val input = List(
      mkEq(Union(TwoList(Cst(37), Cst(38), List(Cst(36)))), Union(TwoList(Var(0), Var(1), List(Var(4), Var(7), Var(16), Var(18))))),
      mkEq(Var(19), Cst(38)),
      mkEq(Var(19), Var(0)),
      mkEq(Var(20), Cst(36)),
      mkEq(Var(20), Var(1)),
      mkEq(Var(23), Cst(38)),
      mkEq(Var(2), Var(23)),
      mkEq(Var(23), Var(22)),
      mkEq(Var(3), Union(TwoList(Var(22), Var(2), List()))),
      mkEq(Var(21), Cst(37)),
      mkEq(Var(4), Union(TwoList(Var(21), Var(3), List()))),
      mkEq(Var(26), Cst(38)),
      mkEq(Var(5), Var(26)),
      mkEq(Var(26), Var(25)),
      mkEq(Var(5), Var(6)),
      mkEq(Var(24), Cst(36)),
      mkEq(Var(7), Union(TwoList(Var(24), Var(6), List()))),
      mkEq(Var(27), Cst(38)),
      mkEq(Var(8), Var(27)),
      mkEq(Var(8), Var(9)),
      mkEq(Var(31), Cst(38)),
      mkEq(Var(10), Var(31)),
      mkEq(Var(31), Var(30)),
      mkEq(Var(11), Union(TwoList(Var(30), Var(10), List()))),
      mkEq(Var(29), Cst(37)),
      mkEq(Var(12), Union(TwoList(Var(29), Var(11), List()))),
      mkEq(Var(24), Var(28)),
      mkEq(Var(13), Union(TwoList(Var(28), Var(12), List()))),
      mkEq(Var(14), Empty),
      mkEq(Var(15), Union(TwoList(Var(32), Var(14), List()))),
      mkEq(Var(32), Union(TwoList(Var(9), Var(13), List(Var(15))))),
      mkEq(Var(33), Var(32)),
      mkEq(Var(33), Var(16)),
      mkEq(Var(35), Cst(38)),
      mkEq(Var(17), Var(35)),
      mkEq(Var(34), Cst(36)),
      mkEq(Var(34), Cst(36)),
      mkEq(Var(34), Var(24)),
      mkEq(Var(34), Cst(36)),
      mkEq(Var(18), Union(TwoList(Var(17), Var(34), List()))),
      mkEq(Var(20), Cst(36))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

  test("Nec.zipWith") {
    val input = List(
      mkEq(Var(20), Cst(35)),
      mkEq(Var(0), Empty),
      mkEq(Var(1), Empty),
      mkEq(Var(22), Empty),
      mkEq(Var(22), Var(2)),
      mkEq(Var(21), Cst(35)),
      mkEq(Var(3), Union(TwoList(Var(21), Var(2), List()))),
      mkEq(Var(4), Empty),
      mkEq(Var(5), Union(TwoList(Var(23), Var(4), List()))),
      mkEq(Var(25), Empty),
      mkEq(Var(25), Var(6)),
      mkEq(Var(24), Cst(35)),
      mkEq(Var(7), Union(TwoList(Var(24), Var(6), List()))),
      mkEq(Var(8), Empty),
      mkEq(Var(23), Var(26)),
      mkEq(Var(9), Union(TwoList(Var(26), Var(8), List()))),
      mkEq(Var(28), Empty),
      mkEq(Var(28), Var(10)),
      mkEq(Var(27), Cst(35)),
      mkEq(Var(11), Union(TwoList(Var(27), Var(10), List()))),
      mkEq(Var(12), Empty),
      mkEq(Var(23), Var(29)),
      mkEq(Var(13), Union(TwoList(Var(29), Var(12), List()))),
      mkEq(Var(31), Empty),
      mkEq(Var(31), Var(14)),
      mkEq(Var(30), Cst(35)),
      mkEq(Var(15), Union(TwoList(Var(30), Var(14), List()))),
      mkEq(Var(16), Empty),
      mkEq(Var(23), Var(32)),
      mkEq(Var(17), Union(TwoList(Var(32), Var(16), List()))),
      mkEq(Var(33), Var(18)),
      mkEq(Var(23), Cst(35)),
      mkEq(Var(33), Union(TwoList(Var(0), Var(1), List(Var(3), Var(5), Var(7), Var(9), Var(11), Var(13), Var(15), Var(18))))),
      mkEq(Var(17), Cst(35)),
      mkEq(Var(34), Var(33)),
      mkEq(Var(19), Var(17)),
      mkEq(Var(34), Var(20))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

  test("RedBlackTree.findLeft") {
    val input = List(
      mkEq(Cst(19), Union(TwoList(Var(6), Var(18), List()))),
      mkEq(Var(8), Empty),
      mkEq(Var(8), Var(0)),
      mkEq(Var(7), Cst(19)),
      mkEq(Var(1), Union(TwoList(Var(7), Var(0), List()))),
      mkEq(Var(9), Var(2)),
      mkEq(Var(11), Var(3)),
      mkEq(Var(13), Var(11)),
      mkEq(Union(TwoList(Var(2), Var(10), List())), Union(TwoList(Var(1), Var(3), List(Var(12))))),
      mkEq(Var(13), Var(4)),
      mkEq(Var(9), Var(14)),
      mkEq(Var(14), Var(5)),
      mkEq(Var(11), Union(TwoList(Var(4), Var(5), List(Var(15))))),
      mkEq(Var(9), Union(TwoList(Var(2), Var(10), List()))),
      mkEq(Var(17), Var(11)),
      mkEq(Var(16), Union(TwoList(Var(2), Var(10), List()))),
      mkEq(Var(17), Var(6))
    )
    val (eqs, _) = SetUnification.solve(input)
    assert(eqs.isEmpty)
  }

}
