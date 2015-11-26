package ca.uwaterloo.flix

import java.nio.file.Paths

import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Algorithms                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("Cyclic.flix") {
    val model = Flix.mkPath(Paths.get("./examples/algorithms/Cyclic.flix"))
    assert(model.isSuccess)
  }

  test("Reachability.flix") {
    val model = Flix.mkPath(Paths.get("./examples/algorithms/Reachability.flix"))
    assert(model.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Analysis                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Belnap.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Belnap.flix"))
    assert(model.isSuccess)
  }

  ignore("Constant.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Constant.flix"))
    assert(model.isSuccess)
  }

  test("Divisor.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Divisor.flix"))
    assert(model.isSuccess)
  }

  ignore("IDE.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/IDE.flix"))
    assert(model.isSuccess)
  }

  test("IFDS.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/IFDS.flix"))
    assert(model.isSuccess)
  }

  test("Interval.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Interval.flix"))
    assert(model.isSuccess)
  }

  test("Parity.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Parity.flix"))
    assert(model.isSuccess)
  }

  test("Sign.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Sign.flix"))
    assert(model.isSuccess)
  }

  test("SU.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/SU.flix"))
    assert(model.isSuccess)
  }

  test("Taint.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Taint.flix"))
    assert(model.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Models                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Bank.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Bank.flix"))
    assert(model.isSuccess)
  }

  ignore("Cards.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Cards.flix"))
    assert(model.isSuccess)
  }

  test("Cinema.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Cinema.flix"))
    assert(model.isSuccess)
  }

  test("Company.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Company.flix"))
    assert(model.isSuccess)
  }

  test("Hotel.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Hotel.flix"))
    assert(model.isSuccess)
  }

  test("Library.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Library.flix"))
    assert(model.isSuccess)
  }

  test("Manufacturer.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Manufacturer.flix"))
    assert(model.isSuccess)
  }

  test("Realtor.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Realtor.flix"))
    assert(model.isSuccess)
  }

  test("Tournament.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/Tournament.flix"))
    assert(model.isSuccess)
  }

  ignore("University.flix") {
    val model = Flix.mkPath(Paths.get("./examples/models/University.flix"))
    assert(model.isSuccess)
  }

}
