package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, SveAlgorithm}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.Zhegalkin.ZhegalkinExpr
import org.scalatest.funsuite.AnyFunSuite

class TestZhegalkin extends AnyFunSuite with TestUtils {

  implicit val alg: BoolAlg[ZhegalkinExpr] = Zhegalkin.ZhegalkinAlgebra

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

  test("Test.Zhegalkin.Failure.01") {
    val f1 = alg.mkBot
    val f2 = alg.mkTop

    assert(SveAlgorithm.unify(f1, f2, Set.empty).isEmpty)
  }

}
