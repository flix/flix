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

  test("Cross11") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |
        |rel R(x: Int, y: Int);
        |
        |A(1, 2). A(3, 4).
        |B(2, 3). B(4, 5).
        |
        |R(x, z) :- A(x, y), B(y, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val R = model.relations(NameR)
    assert(R contains List(Value.mkInt(3), Value.mkInt(5)))
  }

  test("Cross12") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |rel C(x: Int, y: Int);
        |
        |rel R(x: Int, y: Int);
        |
        |A(1, 2). A(3, 4).
        |B(2, 3). B(4, 5).
        |C(3, 7).
        |
        |R(x, w) :- A(x, y), B(y, z), C(z, w).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val R = model.relations(NameR)
    assert(R contains List(Value.mkInt(1), Value.mkInt(7)))
  }

  test("Cross13") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |rel C(x: Int, y: Int);
        |
        |A(1, 2). A(7, 1)
        |B(2, 3). B(3, 5).
        |C(3, 4). C(5, 6).
        |
        |C(x, z) :- A(x, y), B(y, z).
        |A(x, z) :- C(x, y), B(y, z).
        |B(x, z) :- A(x, y), C(y, z).
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val A = model.relations(NameA)
    val B = model.relations(NameB)
    val C = model.relations(NameC)
    assert(C contains List(Value.mkInt(1), Value.mkInt(3)))
    assert(A contains List(Value.mkInt(1), Value.mkInt(5)))
    assert(B contains List(Value.mkInt(1), Value.mkInt(6)))
    assert(C contains List(Value.mkInt(7), Value.mkInt(6)))
  }

  test("NotEqual01") {
    val s =
      """rel A(x: Int);
        |rel B(x: Int, y: Int);
        |
        |A(1). A(2). A(3). A(4).
        |
        |B(x, y) :- A(x), A(y), x != y.
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val B = model.relations(NameB)
    assert(B contains List(Value.mkInt(1), Value.mkInt(2)))
    assert(B contains List(Value.mkInt(1), Value.mkInt(3)))
    assert(B contains List(Value.mkInt(1), Value.mkInt(4)))
    assert(B contains List(Value.mkInt(4), Value.mkInt(1)))
    assert(!(B contains List(Value.mkInt(1), Value.mkInt(1))))
    assert(!(B contains List(Value.mkInt(2), Value.mkInt(2))))
    assert(!(B contains List(Value.mkInt(3), Value.mkInt(3))))
    assert(!(B contains List(Value.mkInt(4), Value.mkInt(4))))
  }

  test("NotEqual02") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 2).
        |
        |B(x, y) :- A(x, y), x != y.
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val B = model.relations(NameB)
    assert(B contains List(Value.mkInt(1), Value.mkInt(2)))
    assert(!(B contains List(Value.mkInt(2), Value.mkInt(2))))
  }

  test("NotEqual03") {
    val s =
      """rel A(x: Int, y: Int);
        |rel B(x: Int, y: Int);
        |
        |A(1, 2).
        |A(2, 1).
        |A(2, 3).
        |
        |B(x, z) :- A(x, y), A(y, z), x != z.
      """.stripMargin

    val model = Flix.fromStrings(s).get
    val B = model.relations(NameB)
    assert(B contains List(Value.mkInt(1), Value.mkInt(3)))
    assert(!(B contains List(Value.mkInt(1), Value.mkInt(1))))
  }

}
