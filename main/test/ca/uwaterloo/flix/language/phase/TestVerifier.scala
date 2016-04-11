package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{Options, Verify}
import org.scalatest.FunSuite

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
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Constant.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  //
  //  test("ConstantSign.flix") {
  //    new Flix()
  //      .addPath("./examples/domains/Belnap.flix")
  //      .addPath("./examples/domains/ConstantSign.flix")
  //      .setOptions(Options.Default.copy(verify = Verify.Enabled))
  //      .solve()
  //      .get
  //  }
  //
  //  ignore("Interval.flix") {
  //    new Flix()
  //      .addPath("./examples/domains/Belnap.flix")
  //      .addPath("./examples/domains/Interval.flix")
  //      .setOptions(Options.Default.copy(verify = Verify.Enabled))
  //      .solve()
  //      .get
  //  }
  //
  test("Parity.flix") {
    new Flix()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Parity.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("Sign.flix") {
    new Flix()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Sign.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("StrictSign.flix") {
    new Flix()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/StrictSign.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }
  //
  //  test("Type.flix") {
  //    new Flix()
  //      .addPath("./examples/domains/Type.flix")
  //      .setOptions(Options.Default.copy(verify = Verify.Enabled))
  //      .solve()
  //      .get
  //  }

  test("ilo/Cube.flix") {
    new Flix()
      .addPath("./examples/domains/ilo/Cube.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("ilo/M2.flix") {
    new Flix()
      .addPath("./examples/domains/ilo/M2.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("ilo/M3.flix") {
    new Flix()
      .addPath("./examples/domains/ilo/M3.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("ilo/M2M3.flix") {
    new Flix()
      .addPath("./examples/domains/ilo/M2M3.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("ilo/SubD4.flix") {
    new Flix()
      .addPath("./examples/domains/ilo/SubD4.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

  test("ilo/SubZ2Z4.flix") {
    new Flix()
      .addPath("./examples/domains/ilo/SubZ2Z4.flix")
      .setOptions(Options.Default.copy(verify = Verify.Enabled))
      .solve()
      .get
  }

}
