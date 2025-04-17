package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.{Options, VerificationOptions}
import org.scalatest.funsuite.AnyFunSuite

class TestVerifiers extends AnyFunSuite with TestUtils {

  test("VerifyStandardLib"){
    // Currently EffectVerifier cannot verify, so skip that one.
    val verifiers = VerificationOptions.Verifiers.all - VerificationOptions.Verifiers.EffectVerifier
    val res = compile("", Options.DefaultTest.copy(xverify = VerificationOptions.EnableSome(verifiers)))
    expectSuccess(res)
  }

}
