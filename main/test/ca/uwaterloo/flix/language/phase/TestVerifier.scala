package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{Options, Verify}
import org.scalatest.FunSuite

// NB: This class may require additional stack space: -Xss16m

class TestVerifier extends FunSuite {

  test("Belnap.flix") {
    new Flix()
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("Constant.flix") {
    new Flix()
      .addPath("./examples/domains/Constant.flix")
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("ConstantSign.flix") {
    new Flix()
      .addPath("./examples/domains/ConstantSign.flix")
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("Interval.flix") {
    new Flix()
      .addPath("./examples/domains/Interval.flix")
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("Parity.flix") {
    new Flix()
      .addPath("./examples/domains/Parity.flix")
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("Sign.flix") {
    new Flix()
      .addPath("./examples/domains/Sign.flix")
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("SignAlt.flix") {
    new Flix()
      .addPath("./examples/domains/SignAlt.flix")
      .addPath("./examples/domains/Belnap.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("Type.flix") {
    new Flix()
      .addPath("./examples/domains/Type.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

}
