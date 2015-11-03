package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.Flix
import ca.uwaterloo.flix.language.ast.Name
import org.scalatest.FunSuite

class TestSolver extends FunSuite {

  val NameA = Name.Resolved(List("A"))

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

}
