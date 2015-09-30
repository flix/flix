package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.Compiler

import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  test("Bank.flix") {
    Compiler.compile(Paths.get("./examples/models/Bank.flix"))
  }

  test("Company.flix") {
    Compiler.compile(Paths.get("./examples/Company.flix"))
  }

  test("University.flix") {
    Compiler.compile(Paths.get("./examples/University.flix"))
  }

}
