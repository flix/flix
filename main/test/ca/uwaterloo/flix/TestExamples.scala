package ca.uwaterloo.flix

import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Algorithms                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("Cyclic.flix") {
    Flix.solve("./examples/algorithms/Cyclic.flix")
  }

  test("Reachability.flix") {
    Flix.solve("./examples/algorithms/Reachability.flix")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Analysis                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Belnap.flix") {
    Flix.solve("./examples/analysis/Belnap.flix")
  }

  test("Constant.flix") {
    Flix.solve("./examples/analysis/Constant.flix")
  }

  test("Divisor.flix") {
    Flix.solve("./examples/analysis/Divisor.flix")
  }

  test("IDE.flix") {
    Flix.solve("./examples/analysis/IDE.flix")
  }

  test("IFDS.flix") {
    Flix.solve("./examples/analysis/IFDS.flix")
  }

  test("Interval.flix") {
    Flix.solve("./examples/analysis/Interval.flix")
  }

  test("Parity.flix") {
    Flix.solve("./examples/analysis/Parity.flix")
  }

  test("Sign.flix") {
    Flix.solve("./examples/analysis/Sign.flix")
  }

  test("SU.flix") {
    Flix.solve("./examples/analysis/SU.flix")
  }

  test("Taint.flix") {
    Flix.solve("./examples/analysis/Taint.flix")
  }

  /////////////////////////////////////////////////////////////////////////////
  // Models                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Bank.flix") {
    Flix.solve("./examples/models/Bank.flix")
  }

  test("Cards.flix") {
    Flix.solve("./examples/models/Cards.flix")
  }

  test("Cinema.flix") {
    Flix.solve("./examples/models/Cinema.flix")
  }

  test("Company.flix") {
    Flix.solve("./examples/models/Company.flix")
  }

  test("Hotel.flix") {
    Flix.solve("./examples/models/Hotel.flix")
  }

  test("Library.flix") {
    Flix.solve("./examples/models/Library.flix")
  }

  test("Manufacturer.flix") {
    Flix.solve("./examples/models/Manufacturer.flix")
  }

  test("Realtor.flix") {
    Flix.solve("./examples/models/Realtor.flix")
  }

  test("Tournament.flix") {
    Flix.solve("./examples/models/Tournament.flix")
  }

  test("University.flix") {
    Flix.solve("./examples/models/University.flix")
  }

}
