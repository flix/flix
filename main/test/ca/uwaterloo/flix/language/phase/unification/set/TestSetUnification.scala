/*
 * Copyright 2024 Jonathan Lindegaard Starup
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.{Compl, Cst, ElemSet, Empty, Inter, Union, Var}
import ca.uwaterloo.flix.language.phase.unification.shared.{CofiniteIntSet, SveMode}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra, ZhegalkinExpr}
import ca.uwaterloo.flix.util.collection.TwoList
import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.SortedSet

class TestSetUnification extends AnyFunSuite with TestUtils {

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
    assertSolved(input)
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
    assertSolved(input)
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
    assertSolved(input)
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
    assertSolved(input)
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
    assertSolved(input)
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
    assertSolved(input)
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
    assertSolved(input)
  }

  test("DelayMap.insertWith.Fusion") {
    // c0 ~ x10 ∪ x11 ∪ c0 ∪ (x8 ∪ x9 ∪ (x6 ∪ x7 ∪ (x4 ∪ x5 ∪ (x2 ∪ x3 ∪ (x0 ∪ x1)))))
    val inner = Union(TwoList(Var(0), Var(1), List()))
    val nested = List(2, 4, 6, 8).foldLeft(inner: SetFormula) {
      case (acc, i) => Union(TwoList(Var(i), Var(i + 1), List(acc)))
    }
    val input = List(
      mkEq(Cst(12), Union(TwoList(Var(10), Var(11), List(Cst(12), nested))))
    )
    assertSolved(input)
  }

  test("CoverAssignment.01") {
    // c1 ~ c1 ∪ x0 ∪ (x1 ∩ c2)
    val input = List(
      mkEq(Cst(1), Union(TwoList(Cst(1), Var(0), List(Inter(TwoList(Var(1), Cst(2), List()))))))
    )
    assertSolved(input)
  }

  test("CoverAssignment.02") {
    // x0 ∪ (c1 ∪ x1) ~ c1, with the ground member in a nested union
    val input = List(
      mkEq(Union(TwoList(Var(0), Union(TwoList(Cst(1), Var(1), List())), List())), Cst(1))
    )
    assertSolved(input)
  }

  test("CoverAssignment.03") {
    // (c1 ∪ c2) ~ (c1 ∪ x0 ∪ c2 ∪ (x1 ∩ c3)): the ground side is a union whose members all occur on the right
    val input = List(
      mkEq(Union(TwoList(Cst(1), Cst(2), List())), Union(TwoList(Cst(1), Var(0), List(Cst(2), Inter(TwoList(Var(1), Cst(3), List()))))))
    )
    assertSolved(input)
  }

  test("Fixpoint.RestrictionThenDistribution") {
    // c4 ~ x1 ∪ x2 restricts x2 to a union; x2 ∩ c3 ~ empty then needs distribution and a bound in a second pass.
    val input = List(
      mkEq(Inter(TwoList(Var(2), Cst(3), List())), Empty),
      mkEq(Cst(4), Union(TwoList(Var(1), Var(2), List())))
    )
    assertSolved(input)
  }

  test("InterAssignment.01") {
    // c1 ~ x0 ∩ (c1 ∪ c2)
    val input = List(
      mkEq(Cst(1), Inter(TwoList(Var(0), Union(TwoList(Cst(1), Cst(2), List())), List())))
    )
    assertSolved(input)
  }

  test("InterAssignment.02") {
    // x0 ∩ c1 ~ c1, i.e. the restricted variable must cover c1
    val input = List(
      mkEq(Inter(TwoList(Var(0), Cst(1), List())), Cst(1))
    )
    assertSolved(input)
  }

  test("InterAssignment.03") {
    // c1 ~ x0 ∪ x1 restricts x0 and x1 to c1; c1 ~ x0 then equates the restricted x0 to c1.
    val input = List(
      mkEq(Cst(1), Union(TwoList(Var(0), Var(1), List()))),
      mkEq(Cst(1), Var(0))
    )
    assertSolved(input)
  }

  test("BoundAssignment.Nested") {
    // e1 ∩ ((!x0 ∩ !c2) ∩ !c2) ~ empty: the variable sits in a nested intersection
    val input = List(
      mkEq(Inter(TwoList(ElemSet(SortedSet(1)), Inter(TwoList(Inter(TwoList(Compl(Var(0)), Compl(Cst(2)), List())), Compl(Cst(2)), List())), List())), Empty)
    )
    assertSolved(input)
  }

  test("BoundAssignment.Complementary") {
    // e1 ∩ !x0 ∩ !e1 ~ empty holds for every x0
    val input = List(
      mkEq(Inter(TwoList(ElemSet(SortedSet(1)), Compl(Var(0)), List(Compl(ElemSet(SortedSet(1)))))), Empty)
    )
    assertSolved(input)
  }

  test("BoundAssignment.MutualDependency") {
    // Two inclusions whose bounds mention each other's variable, as the lambdas of a region produce.
    val e = ElemSet(SortedSet(1))
    val input = List(
      mkEq(Inter(TwoList(e, Compl(Var(0)), List(Compl(Cst(2)), Compl(Var(1))))), Empty),
      mkEq(Inter(TwoList(e, Compl(Var(1)), List(Compl(Cst(2)), Compl(Var(0))))), Empty),
      mkEq(Inter(TwoList(e, Compl(Var(1)), List(Compl(Cst(2)), Compl(Var(0)), Compl(Var(3))))), Empty),
      mkEq(Var(3), Union(TwoList(Var(0), Cst(2), List())))
    )
    assertSolved(input)
  }

  test("ConstantPropagation.ComplVar") {
    // !x0 ~ empty
    val input = List(
      mkEq(Compl(Var(0)), Empty)
    )
    assertSolved(input)
  }

  /**
    * Asserts that `input` is solved by SVE at every granularity, and that the substitution returned
    * satisfies every equation of `input`.
    */
  def assertSolved(input: List[Equation]): Unit = {
    for (mode <- List(SveMode.PerEquation, SveMode.PerComponent, SveMode.PerSystem)) {
      val (eqs, subst) = SetUnification.solve(input, mode)
      assert(eqs.isEmpty, mode)
      for (eq <- input) {
        assert(isEquivalent(subst(eq.f1), subst(eq.f2)), s"$mode: $eq is not satisfied by $subst")
      }
    }
  }

  /** Returns `true` if `f1` and `f2` denote the same set for every value of their variables and constants. */
  private def isEquivalent(f1: SetFormula, f2: SetFormula): Boolean = {
    implicit val alg: ZhegalkinAlgebra[CofiniteIntSet] = EffUnification3.Algebra
    implicit val lat: CofiniteIntSet.LatticeOps.type = CofiniteIntSet.LatticeOps
    val z1 = Zhegalkin.toZhegalkin(f1)
    val z2 = Zhegalkin.toZhegalkin(f2)
    ZhegalkinExpr.isEmpty(ZhegalkinExpr.mkXor(z1, z2))
  }

}
