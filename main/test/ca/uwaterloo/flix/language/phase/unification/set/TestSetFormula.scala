/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.phase.unification.EffUnification3
import ca.uwaterloo.flix.language.phase.unification.set.SetFormula.{Compl, Cst, Empty, Inter, Union, Univ, Var, simplify}
import ca.uwaterloo.flix.language.phase.unification.shared.CofiniteIntSet
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.{Zhegalkin, ZhegalkinAlgebra, ZhegalkinExpr}
import ca.uwaterloo.flix.util.collection.TwoList
import org.scalatest.funsuite.AnyFunSuite

class TestSetFormula extends AnyFunSuite with TestUtils {

  private val a: SetFormula = Var(0)
  private val b: SetFormula = Cst(1)
  private val c: SetFormula = Cst(2)

  private def inter(fs: SetFormula*): SetFormula = Inter(TwoList(fs.head, fs(1), fs.drop(2).toList))

  private def union(fs: SetFormula*): SetFormula = Union(TwoList(fs.head, fs(1), fs.drop(2).toList))

  test("Simplify.Atoms") {
    assert(simplify(a) == a)
    assert(simplify(Compl(a)) == Compl(a))
    assert(simplify(Univ) == Univ)
    assert(simplify(Empty) == Empty)
  }

  test("Simplify.Units") {
    assert(simplify(inter(Univ, a)) == a)
    assert(simplify(union(Empty, a)) == a)
    assert(simplify(inter(Empty, a)) == Empty)
    assert(simplify(union(Univ, a)) == Univ)
  }

  test("Simplify.Flatten") {
    assert(simplify(inter(inter(a, b), c)) == inter(a, b, c))
    assert(simplify(union(a, union(b, c))) == union(a, b, c))
  }

  test("Simplify.Duplicates") {
    assert(simplify(inter(a, b, a)) == inter(a, b))
    assert(simplify(union(a, a)) == a)
    assert(simplify(inter(inter(a, b), a)) == inter(a, b))
  }

  test("Simplify.Complementary") {
    assert(simplify(inter(a, Compl(a))) == Empty)
    assert(simplify(union(a, Compl(a))) == Univ)
    assert(simplify(inter(b, Compl(a), Compl(b))) == Empty)
  }

  test("Simplify.Absorption") {
    assert(simplify(inter(a, union(a, b))) == a)
    assert(simplify(union(a, inter(a, b))) == a)
    assert(simplify(inter(union(a, b), a, c)) == inter(a, c))
  }

  test("Simplify.DeMorgan") {
    assert(simplify(Compl(union(a, b))) == inter(Compl(a), Compl(b)))
    assert(simplify(Compl(inter(a, Compl(a)))) == Univ)
  }

  test("Simplify.Idempotent") {
    val fs = List(inter(inter(a, b), a, union(a, c)), union(Compl(a), inter(a, b), Empty), Compl(union(inter(a, b), Compl(b))))
    for (f <- fs) {
      val s = simplify(f)
      assert(simplify(s) == s, f)
    }
  }

  test("Simplify.Sound") {
    val fs = List(inter(inter(a, b), a, union(a, c)), union(Compl(a), inter(a, b), Empty), Compl(union(inter(a, b), Compl(b))), inter(b, Compl(a), Compl(b)), union(a, inter(a, b), c))
    for (f <- fs) {
      assert(isEquivalent(f, simplify(f)), f)
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
