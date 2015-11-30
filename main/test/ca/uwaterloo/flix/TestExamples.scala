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
        |    A(3, Belnap.True).
        |    A(3, Belnap.False).
        |
        |    A(4, x) :- A(1, x), A(2, x).
        |
        |    A(5, not(Belnap.False)).
        |
        |    A(6, Belnap.True `and` Belnap.False).
        |
        |    A(7, Belnap.True `or` Belnap.False).
        |
        |    A(8, Belnap.True `xor` Belnap.False).
        |}
      """.stripMargin

    val model = Flix.mkStr(List(input1, input2)).get

    val Belnap = Name.Resolved.mk(List("Belnap", "Belnap"))

    val Tru = Value.mkTag(Belnap, "True", Value.Unit)
    val Fls = Value.mkTag(Belnap, "False", Value.Unit)
    val Top = Value.mkTag(Belnap, "Top", Value.Unit)

    val A = model.lattices(Name.Resolved.mk(List("Belnap", "A"))).toMap

    assertResult(List(Tru))(A(List(Value.mkInt(1))))
    assertResult(List(Fls))(A(List(Value.mkInt(2))))
    assertResult(List(Top))(A(List(Value.mkInt(3))))
    assertResult(None)(A.get(List(Value.mkInt(4))))
    assertResult(List(Tru))(A(List(Value.mkInt(5))))
    assertResult(List(Fls))(A(List(Value.mkInt(6))))
    assertResult(List(Tru))(A(List(Value.mkInt(7))))
    assertResult(List(Tru))(A(List(Value.mkInt(8))))
  }

  test("Parity.flix") {
    val input1 = Source.fromFile("./examples/domains/Parity.flix").getLines().mkString("\n")
    val input2 =
      """namespace Parity {
        |    let Parity<> = (Parity.Bot, Parity.Top, leq, lub, glb);
        |    lat A(k: Int, v: Parity<>);
        |
        |    A(1, Parity.Odd).
        |    A(2, Parity.Even).
        |
        |    A(3, Parity.Odd).
        |    A(3, Parity.Even).
        |
        |    A(4, x) :- A(1, x), A(2, x).
        |
        |    A(5, Parity.Odd `plus` Parity.Even).
        |
        |    A(6, Parity.Odd `plus` Parity.Odd).
        |
        |    A(7, Parity.Odd `times` Parity.Even).
        |
        |    A(8, Parity.Odd `times` Parity.Odd).
        |}
      """.stripMargin

    val model = Flix.mkStr(List(input1, input2)).get

    val Parity = Name.Resolved.mk(List("Parity", "Parity"))

    val Odd = Value.mkTag(Parity, "Odd", Value.Unit)
    val Evn = Value.mkTag(Parity, "Even", Value.Unit)
    val Top = Value.mkTag(Parity, "Top", Value.Unit)

    val A = model.lattices(Name.Resolved.mk(List("Parity", "A"))).toMap

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
