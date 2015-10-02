package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.Compiler

import org.scalatest.FunSuite

class TestResolver extends FunSuite {

  test("DuplicateDefinition01") {
    val input =
      s"""namespace A {
         |  val foo: Int = 42;
         |
         |  val foo: Int = 21;
         |};
       """.stripMargin
    val result = Compiler.compile(input)
    assert(result.hasErrors)
  }

  test("DuplicateDefinition02") {
    val input =
      s"""namespace A {
         |  val foo: Unit = ();
         |
         |  val foo: Bool = true;
         |
         |  val foo: Int = 42;
         |
         |  val foo: Str = "bar";
         |};
       """.stripMargin
    val result = Compiler.compile(input)
    assert(result.hasErrors)
  }

  test("DuplicateDefinition03") {
    val input =
      s"""namespace A {
         |  def foo(x: Int): Int = 42;
         |
         |  def foo(x: Int): Int = 21;
         |};
       """.stripMargin
    val result = Compiler.compile(input)
    assert(result.hasErrors)
  }

  test("DuplicateDefinition04") {
    val input =
      s"""namespace A {
         |  val foo: Int = 42;
         |
         |  def foo(x: Int): Int = 21;
         |};
       """.stripMargin
    val result = Compiler.compile(input)
    assert(result.hasErrors)
  }

  test("DuplicateDefinition05") {
    val input =
      s"""namespace A {
         |  val foo: Int = 42;
         |};
         |
         |namespace A {
         |  val foo: Int = 21;
         |};
       """.stripMargin
    val result = Compiler.compile(input)
    assert(result.hasErrors)
  }

  test("DuplicateDefinition06") {
    val input =
      s"""namespace A::B::C {
         |  val foo: Int = 42;
         |};
         |
         |namespace A {
         |  namespace B {
         |    namespace C {
         |      val foo: Int = 21;
         |    }
         |  };
         |};
       """.stripMargin
    val result = Compiler.compile(input)
    assert(result.hasErrors)
  }

}
