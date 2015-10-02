package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler

import org.scalatest.FunSuite

class TestResolver extends FunSuite {

  test("DuplicateDefinition01") {
    val input =
      s"""namespace A {
         |};
       """.stripMargin
    val result = Compiler.compile(input)

  }

}
