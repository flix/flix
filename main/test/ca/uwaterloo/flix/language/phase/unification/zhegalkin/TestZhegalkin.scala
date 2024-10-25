package ca.uwaterloo.flix.language.phase.unification.zhegalkin

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.phase.unification.shared.{BoolAlg, SveAlgorithm}
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.Zhegalkin.ZhegalkinExpr
import org.scalatest.funsuite.AnyFunSuite

class TestZhegalkin extends AnyFunSuite with TestUtils {

  implicit val alg: BoolAlg[ZhegalkinExpr] = Zhegalkin.ZhegalkinAlgebra

  test("Test.Zhegalkin.01") {
    val f1 = alg.mkBot
    val f2 = alg.mkBot

    SveAlgorithm.unify(f1, f2, Set.empty).get
  }

}
