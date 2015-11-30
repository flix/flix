package ca.uwaterloo.flix

import java.nio.file.Paths

import ca.uwaterloo.flix.language.ast.Name
import ca.uwaterloo.flix.runtime.Value
import org.scalatest.FunSuite

import scala.io.Source

class TestExamples extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Domains                                                                 //
  /////////////////////////////////////////////////////////////////////////////
  test("Belnap.flix") {
    val input1 = Source.fromFile("./examples/domains/Belnap.flix").getLines().mkString("\n")
    val input2 =
      """namespace Belnap {
        |    let Belnap<> = (Belnap.Bot, Belnap.Top, leq, lub, glb);
        |    lat A(k: Int, v: Belnap<>);
        |
        |    A(1, Belnap.True).
        |    A(2, Belnap.False).
        |
        |    A(3, and(Belnap.True, Belnap.False)).
        |    A(4, v1 `or` v2) :- A(1, v1), A(2, v2).
        |    A(5, Belnap.True).
        |    A(5, Belnap.False).
        |
        |assert A(3, Belnap.False).
        |assert A(4, Belnap.True).
        |assert A(5, Belnap.Top).
        |
        |print A.
        |}
      """.stripMargin
    val model = Flix.mkStr(List(input1, input2)).get
    val Belnap = Name.Resolved.mk(List("Belnap", "Belnap"))

    val A = model.lattices(Name.Resolved.mk(List("Belnap", "A"))).toMap

    assertResult(List(Value.mkTag(Belnap, "True", Value.Unit)))(A(List(Value.mkInt(1))))
    assertResult(List(Value.mkTag(Belnap, "False", Value.Unit)))(A(List(Value.mkInt(2))))
  }


  /////////////////////////////////////////////////////////////////////////////
  // Misc                                                                    //
  /////////////////////////////////////////////////////////////////////////////
  ignore("Constant.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Constant.flix"))
    assert(model.isSuccess)
  }

  ignore("IDE.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/IDE.flix"))
    assert(model.isSuccess)
  }

  test("IFDS.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/IFDS.flix"))
    assert(model.isSuccess)
  }

  test("Interval.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Interval.flix"))
    assert(model.isSuccess)
  }

  test("Parity.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Parity.flix"))
    assert(model.isSuccess)
  }

  test("Sign.flix") {
    val model = Flix.mkPath(Paths.get("./examples/analysis/Sign.flix"))
    assert(model.isSuccess)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Entities                                                                //
  /////////////////////////////////////////////////////////////////////////////

  test("Bank.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Bank.flix"))
    assert(model.isSuccess)
  }

  ignore("Cards.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Cards.flix"))
    assert(model.isSuccess)
  }

  test("Cinema.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Cinema.flix"))
    assert(model.isSuccess)
  }

  test("Company.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Company.flix"))
    assert(model.isSuccess)
  }

  test("Hotel.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Hotel.flix"))
    assert(model.isSuccess)
  }

  test("Library.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Library.flix"))
    assert(model.isSuccess)
  }

  test("Manufacturer.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Manufacturer.flix"))
    assert(model.isSuccess)
  }

  test("Realtor.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Realtor.flix"))
    assert(model.isSuccess)
  }

  test("Tournament.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/Tournament.flix"))
    assert(model.isSuccess)
  }

  ignore("University.flix") {
    val model = Flix.mkPath(Paths.get("./examples/entities/University.flix"))
    assert(model.isSuccess)
  }

}
