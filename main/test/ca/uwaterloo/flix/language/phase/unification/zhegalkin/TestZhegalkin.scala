package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, SveAlgorithm}
import org.scalatest.funsuite.AnyFunSuite

class TestZhegalkin extends AnyFunSuite with TestUtils {

  implicit val alg: BoolAlg[ZhegalkinExpr] = ZhegalkinAlgebra

  test("Test.Zhegalkin.Success.01") {
    val f1 = alg.mkBot
    val f2 = alg.mkBot

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.02") {
    val f1 = alg.mkVar(1)
    val f2 = alg.mkBot

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.03") {
    val f1 = alg.mkBot
    val f2 = alg.mkVar(1)

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.04") {
    val f1 = alg.mkVar(1)
    val f2 = alg.mkTop

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.05") {
    val f1 = alg.mkTop
    val f2 = alg.mkVar(1)

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.06") {
    val f1 = alg.mkVar(1)
    val f2 = alg.mkVar(2)

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.07") {
    val f1 = alg.mkVar(1)
    val f2 = alg.mkOr(alg.mkVar(2), alg.mkVar(3))

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Success.08") {
    val f1 = alg.mkOr(alg.mkVar(1), alg.mkVar(2))
    val f2 = alg.mkOr(alg.mkOr(alg.mkVar(3), alg.mkVar(4)), alg.mkVar(5))

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

  test("Test.Zhegalkin.Failure.01") {
    val f1 = alg.mkBot
    val f2 = alg.mkTop

    assert(SveAlgorithm.unify(f1, f2, Set.empty).isEmpty)
  }

}
