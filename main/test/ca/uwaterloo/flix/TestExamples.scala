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
    val input2 = Source.fromFile("./examples/domains/Belnap.flix").getLines().mkString("\n")
    val input3 =
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

    val model = Flix.mkStr(List(input1, input2, input3)).get

    val Parity = Name.Resolved.mk(List("Parity", "Parity"))

    val Odd = Value.mkTag(Parity, "Odd", Value.Unit)
    val Evn = Value.mkTag(Parity, "Even", Value.Unit)
    val Top = Value.mkTag(Parity, "Top", Value.Unit)

    val A = model.lattices(Name.Resolved.mk(List("Parity", "A"))).toMap

    assertResult(List(Odd))(A(List(Value.mkInt(1))))
    assertResult(List(Evn))(A(List(Value.mkInt(2))))
    assertResult(List(Top))(A(List(Value.mkInt(3))))
    assertResult(None)(A.get(List(Value.mkInt(4))))
    assertResult(List(Odd))(A(List(Value.mkInt(5))))
    assertResult(List(Evn))(A(List(Value.mkInt(6))))
    assertResult(List(Evn))(A(List(Value.mkInt(7))))
    assertResult(List(Odd))(A(List(Value.mkInt(8))))
  }

  test("Sign.flix") {
    val input1 = Source.fromFile("./examples/domains/Sign.flix").getLines().mkString("\n")
    val input2 = Source.fromFile("./examples/domains/Belnap.flix").getLines().mkString("\n")
    val input3 =
      """namespace Sign {
        |    let Sign<> = (Sign.Bot, Sign.Top, leq, lub, glb);
        |    lat A(k: Int, v: Sign<>);
        |
        |    A(1, Sign.Neg).
        |    A(2, Sign.Zer).
        |    A(3, Sign.Pos).
        |
        |    A(4, Sign.Neg).
        |    A(4, Sign.Zer).
        |    A(4, Sign.Pos).
        |
        |    A(5, x) :- A(1, x), A(2, x), A(3, x).
        |
        |    A(6, Sign.Zer `plus` Sign.Pos).
        |    A(7, Sign.Neg `plus` Sign.Pos).
        |
        |    A(8, Sign.Zer `times` Sign.Pos).
        |    A(9, Sign.Neg `times` Sign.Neg).
        |}
      """.stripMargin

    val model = Flix.mkStr(List(input1, input2, input3)).get

    val Sign = Name.Resolved.mk(List("Sign", "Sign"))

    val Neg = Value.mkTag(Sign, "Neg", Value.Unit)
    val Zer = Value.mkTag(Sign, "Zer", Value.Unit)
    val Pos = Value.mkTag(Sign, "Pos", Value.Unit)
    val Top = Value.mkTag(Sign, "Top", Value.Unit)

    val A = model.lattices(Name.Resolved.mk(List("Sign", "A"))).toMap

    assertResult(List(Neg))(A(List(Value.mkInt(1))))
    assertResult(List(Zer))(A(List(Value.mkInt(2))))
    assertResult(List(Pos))(A(List(Value.mkInt(3))))
    assertResult(List(Top))(A(List(Value.mkInt(4))))
    assertResult(None)(A.get(List(Value.mkInt(5))))
    assertResult(List(Pos))(A(List(Value.mkInt(6))))
    assertResult(List(Top))(A(List(Value.mkInt(7))))
    assertResult(List(Zer))(A(List(Value.mkInt(8))))
    assertResult(List(Pos))(A(List(Value.mkInt(9))))
  }

  test("Constant.flix") {
    val input1 = Source.fromFile("./examples/domains/Constant.flix").getLines().mkString("\n")
    val input2 = Source.fromFile("./examples/domains/Belnap.flix").getLines().mkString("\n")
    val input3 =
      """namespace Constant {
        |    let Constant<> = (Constant.Bot, Constant.Top, leq, lub, glb);
        |    lat A(k: Int, v: Constant<>);
        |
        |    A(0, Constant.Cst(0))
        |    A(1, Constant.Cst(1))
        |    A(2, Constant.Cst(2))
        |
        |    A(3, x) :- A(0, x).
        |    A(3, x) :- A(1, x).
        |    A(3, x) :- A(2, x).
        |
        |    A(4, x) :- A(0, x), A(1, x), A(2, x).
        |
        |    A(5, x `plus` y)  :- A(0, x), A(2, y).
        |    A(6, x `times` y) :- A(1, x), A(2, y).
        |}
      """.stripMargin

    val model = Flix.mkStr(List(input1, input2, input3)).get

    val Constant = Name.Resolved.mk(List("Constant", "Constant"))

    val Zer = Value.mkTag(Constant, "Cst", Value.mkInt(0))
    val One = Value.mkTag(Constant, "Cst", Value.mkInt(1))
    val Two = Value.mkTag(Constant, "Cst", Value.mkInt(2))
    val Top = Value.mkTag(Constant, "Top", Value.Unit)

    val A = model.lattices(Name.Resolved.mk(List("Constant", "A"))).toMap

    assertResult(List(Zer))(A(List(Value.mkInt(0))))
    assertResult(List(One))(A(List(Value.mkInt(1))))
    assertResult(List(Two))(A(List(Value.mkInt(2))))
    assertResult(List(Top))(A(List(Value.mkInt(3))))
    assertResult(None)(A.get(List(Value.mkInt(4))))
    assertResult(List(Two))(A(List(Value.mkInt(5))))
    assertResult(List(Two))(A(List(Value.mkInt(6))))
  }

  test("ConstantSign.flix") {
    val input1 = Source.fromFile("./examples/domains/ConstantSign.flix").getLines().mkString("\n")
    val input2 = Source.fromFile("./examples/domains/Belnap.flix").getLines().mkString("\n")
    val input3 =
      """namespace ConstantSign {
        |    let ConstSign<> = (ConstSign.Bot, ConstSign.Top, leq, lub, glb);
        |    lat A(k: Int, v: ConstSign<>);
        |
        |    A(1, ConstSign.Cst(-1))
        |    A(2, ConstSign.Cst(0))
        |    A(3, ConstSign.Cst(1))
        |
        |    A(4, x) :- A(1, x). // 4 -> top
        |    A(4, x) :- A(2, x). // 4 -> top
        |    A(4, x) :- A(3, x). // 4 -> top
        |
        |    A(5, x) :- A(2, x). // 5 -> pos
        |    A(5, x) :- A(3, x). // 5 -> pos
        |
        |    A(6, x) :- A(1, x), A(2, x). // 6 -> bot
        |    A(7, x) :- A(2, x), A(3, x). // 7 -> bot
        |
        |    A(8, x) :- A(4, x), A(5, x). // 8 -> pos
        |
        |    A(9, x `times` y) :- A(1, x), A(1, y). // 9 -> 1
        |}
      """.stripMargin

    val model = Flix.mkStr(List(input1, input2, input3)).get

    val ConstantSign = Name.Resolved.mk(List("ConstantSign", "ConstSign"))

    val Zer = Value.mkTag(ConstantSign, "Cst", Value.mkInt(0))
    val One = Value.mkTag(ConstantSign, "Cst", Value.mkInt(1))
    val Neg = Value.mkTag(ConstantSign, "Neg", Value.Unit)
    val Pos = Value.mkTag(ConstantSign, "Pos", Value.Unit)
    val Top = Value.mkTag(ConstantSign, "Top", Value.Unit)

    val A = model.lattices(Name.Resolved.mk(List("ConstantSign", "A"))).toMap

    assertResult(List(Zer))(A(List(Value.mkInt(2))))
    assertResult(List(One))(A(List(Value.mkInt(3))))
    assertResult(List(Top))(A(List(Value.mkInt(4))))
    assertResult(List(Top))(A(List(Value.mkInt(4))))
    assertResult(List(Pos))(A(List(Value.mkInt(5))))
    assertResult(None)(A.get(List(Value.mkInt(6))))
    assertResult(None)(A.get(List(Value.mkInt(7))))
    assertResult(List(Pos))(A(List(Value.mkInt(8))))
    assertResult(List(One))(A(List(Value.mkInt(9))))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Misc                                                                    //
  /////////////////////////////////////////////////////////////////////////////

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
