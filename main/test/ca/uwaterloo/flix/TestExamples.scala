package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.Compiler

import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Algorithms                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("Cyclic.flix") {
    Compiler.compile(Paths.get("./examples/algorithms/Cyclic.flix"))
  }

  test("Reachability.flix") {
    Compiler.compile(Paths.get("./examples/algorithms/Reachability.flix"))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Analysis                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Belnap.flix") {
    Compiler.compile(Paths.get("./examples/analysis/Belnap.flix"))
  }

  test("Constant.flix") {
    Compiler.compile(Paths.get("./examples/analysis/Constant.flix"))
  }

  test("Interval.flix") {
    Compiler.compile(Paths.get("./examples/analysis/Interval.flix"))
  }

  test("Parity.flix") {
    Compiler.compile(Paths.get("./examples/analysis/Parity.flix"))
  }

  test("Sign.flix") {
    Compiler.compile(Paths.get("./examples/analysis/Sign.flix"))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Models                                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Bank.flix") {
    Compiler.compile(Paths.get("./examples/models/Bank.flix"))
  }

  test("Company.flix") {
    Compiler.compile(Paths.get("./examples/models/Company.flix"))
  }

  test("University.flix") {
    Compiler.compile(Paths.get("./examples/models/University.flix"))
  }

}
