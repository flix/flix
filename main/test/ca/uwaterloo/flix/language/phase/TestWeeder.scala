package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix

import org.scalatest.FunSuite

class TestWeeder extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Alias                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAlias01") {
    val input = "P(x, y) :- A(x), y := 21, y := 42. "
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAlias])
  }

  test("DuplicateAlias02") {
    val input = "P(x, y) :- A(x), y := 21, z := 84, y := 42."
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAlias])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Annotation                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAnnotation01") {
    val input =
      """@strict @strict
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAnnotation])
  }

  test("DuplicateAnnotation02") {
    val input =
      """@strict @monotone @strict @monotone
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAnnotation])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Attribute                                                     //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateAttribute01") {
    val input = "rel A(x: Int, x: Int)"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute02") {
    val input = "rel A(x: Int, y: Int, x: Int)   "
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  test("DuplicateAttribute03") {
    val input = "rel A(x: Bool, x: Int, x: Str)"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateAttribute])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Formal                                                        //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateFormal01") {
    val input = "fn f(x: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  test("DuplicateFormal02") {
    val input = "fn f(x: Int, y: Int, x: Int): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  test("DuplicateFormal03") {
    val input = "fn f(x: Bool, x: Int, x: Str): Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateFormal])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Duplicate Tag                                                           //
  /////////////////////////////////////////////////////////////////////////////
  test("DuplicateTag01") {
    val input =
      """enum Color {
        |  case Red,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateTag])
  }

  test("DuplicateTag02") {
    val input =
      """enum Color {
        |  case Red,
        |  case Blu,
        |  case Red
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.DuplicateTag])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Annotation                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalAnnotation01") {
    val input =
      """@abc
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalAnnotation])
  }

  test("IllegalAnnotation02") {
    val input =
      """@foobarbaz
        |fn foo(x: Int): Int = 42
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalAnnotation])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Body Term                                                       //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalBodyTerm01") {
    val input = "P(x) :- A(f(x))."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalBodyTerm])
  }

  test("IllegalBodyTerm02") {
    val input = "P(x) :- A(x), B(f(x)), C(x)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalBodyTerm])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Index                                                           //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalNoIndex01") {
    val input =
      """rel A(x: Int, y: Int, z: Int)
        |index A()
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.MissingIndex])
  }

  test("IllegalIndex01") {
    val input =
      """rel A(x: Int, y: Int, z: Int)
        |index A({})
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalIndex])
  }

  test("IllegalIndex02") {
    val input =
      """rel A(x: Int, y: Int, z: Int)
        |index A({x}, {}, {z})
      """.stripMargin
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalIndex])
  }

  /////////////////////////////////////////////////////////////////////////////
  // IllegalHeadPredicate                                                    //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalHeadPredicate.Alias01") {
    val input = "x := y."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadPredicate])
  }

  test("IllegalHeadPredicate.Alias02") {
    val input = "x := y :- A(x, y)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadPredicate])
  }

  test("IllegalHeadPredicate.NotEqual01") {
    val input = "x != y."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadPredicate])
  }

  test("IllegalHeadPredicate.NotEqual02") {
    val input = "x != y :- A(x, y)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadPredicate])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Head Term                                                       //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalHeadTerm01") {
    val input = "P(_)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadTerm])
  }

  test("IllegalHeadTerm02") {
    val input = "P(x, _, z) :- A(x, z)."
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalHeadTerm])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Illegal Lattice                                                         //
  /////////////////////////////////////////////////////////////////////////////
  test("IllegalLattice01") {
    val input = "let Foo<> = (1, 2)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLattice])
  }

  test("IllegalLattice02") {
    val input = "let Foo<> = (1, 2, 3, 4, 5, 6, 7, 8, 9)"
    val result = new Flix().addStr(input).solve()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.IllegalLattice])
  }

  /////////////////////////////////////////////////////////////////////////////
  // Non Linear Pattern                                                      //
  /////////////////////////////////////////////////////////////////////////////
  test("NonLinearPattern01") {
    val input =
      """fn f(): Bool = match (21, 42) with {
        |  case (x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.NonLinearPattern])
  }

  test("NonLinearPattern02") {
    val input =
      """fn f(): Bool = match (21, 42, 84) with {
        |  case (x, x, x) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.NonLinearPattern])
  }

  test("NonLinearPattern03") {
    val input =
      """fn f(): Bool = match (1, (2, (3, 4))) with {
        |  case (x, (y, (z, x))) => true
        |}
      """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Weeder.WeederError.NonLinearPattern])
  }

}
