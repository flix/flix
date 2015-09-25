package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.Compiler

import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  test("Company.flix") {
    Compiler.compile(List(Paths.get("./examples/Company.flix")))
  }

  test("University.flix") {
    Compiler.compile(List(Paths.get("./examples/University.flix")))
  }

}
