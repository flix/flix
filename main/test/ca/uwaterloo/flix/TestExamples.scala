package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.Compiler

import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  // TODO: Figure out proper interface for the compiler and runtime.

  /////////////////////////////////////////////////////////////////////////////
  // Algorithms                                                              //
  /////////////////////////////////////////////////////////////////////////////
  test("Cycles.flix") {
    Compiler.compile(Paths.get("./examples/algorithms/Cycles.flix"))
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
