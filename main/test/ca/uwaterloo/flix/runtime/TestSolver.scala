package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.Flix
import ca.uwaterloo.flix.language.ast.Name
import org.scalatest.FunSuite

class TestSolver extends FunSuite {

  val NameA = Name.Resolved(List("A"))
  val NameB = Name.Resolved(List("B"))
  val NameC = Name.Resolved(List("C"))
  val NameR = Name.Resolved(List("R"))

  test("Cross01") {
    val s =
      """rel A(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 3).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(1), Value.mkInt(3)))
  }

  test("Cross02") {
    val s =
      """rel A(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 3).
        |A(3, 4).
        |A(4, 5).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(1), Value.mkInt(5)))
    assert(A contains List(Value.mkInt(1), Value.mkInt(5)))
  }

  test("Cross03") {
    val s =
      """rel A(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 3).
        |A(3, 1).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(2), Value.mkInt(2)))
  }

  test("Cross04") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(1, "b", 2).
        |
        |A(x, c, z) :- A(x, c, y), A(y, c, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(1), Value.mkStr("a"), Value.mkInt(3)))
  }

  test("Cross05") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, "b", y) :- A(x, "a", y).
        |A(x, c, z) :- A(x, c, y), A(y, c, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(2), Value.mkStr("b"), Value.mkInt(2)))
  }

  test("Cross06") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, c, y) :- A(y, c, x).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(1), Value.mkStr("b"), Value.mkInt(3)))
  }

  test("Cross07") {
    val s =
      """rel A(x: Int, y: Str, z: Int);
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, c, y) :- A(y, c, x).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    assert(A contains List(Value.mkInt(1), Value.mkStr("b"), Value.mkInt(3)))
  }

  test("Cross08") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |rel R(x: Int);
        |
        |A(1). A(2).
        |B(2). B(3).
        |
        |R(x) :- A(x), B(x).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val R = model.relations(NameR)
    assert(R contains List(Value.mkInt(2)))
  }

  test("Cross09") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |rel C(x: Int);
        |
        |rel R(x: Int);
        |
        |A(1). A(2). A(3).
        |B(2). B(3).
        |C(3).
        |
        |R(x) :- A(x), B(x), C(x).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val R = model.relations(NameR)
    assert(R contains List(Value.mkInt(3)))
  }

  test("Cross10") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int);
        |rel C(x: Int);
        |
        |rel R(x: Int);
        |
        |A(1). A(2). A(3).
        |B(2). B(3).
        |C(3).
        |
        |R(x) :- C(x), A(x), B(x), B(x), C(x), A(x).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val R = model.relations(NameR)
    assert(R contains List(Value.mkInt(3)))
  }


}
