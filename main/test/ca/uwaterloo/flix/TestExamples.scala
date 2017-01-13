/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.runtime.{Model, Value}
import ca.uwaterloo.flix.util._
import org.scalatest.FunSuite

class TestExamples extends FunSuite {

  private class Tester(dumpBytecode: Boolean = false) {

    private val interpretedFlix = createFlix(codegen = false)
    private val compiledFlix = createFlix(codegen = true)
    private var interpreted: Model = null
    private var compiled: Model = null

    private def createFlix(codegen: Boolean = false) = {
      val options = Options.DefaultTest.copy(evaluation = if (codegen) Evaluation.Compiled else Evaluation.Interpreted)
      new Flix().setOptions(options)
    }

    def addPath(path: String): Tester = {
      interpretedFlix.addPath(path)
      compiledFlix.addPath(path)
      this
    }

    def addStr(str: String): Tester = {
      interpretedFlix.addStr(str)
      compiledFlix.addStr(str)
      this
    }

    def run(): Tester = {
      interpreted = interpretedFlix.solve().get
      compiled = compiledFlix.solve().get
      this
    }

    def checkValue(expected: AnyRef, latticeName: String, key: List[AnyRef]): Unit = {
      withClue(s"interpreted value $latticeName($key):") {
        val lattice = interpreted.getLattice(latticeName).toMap
        assertResult(expected)(lattice(key))
      }
      withClue(s"compiled value $latticeName($key):") {
        val lattice = compiled.getLattice(latticeName).toMap
        assertResult(expected)(lattice(key))
      }
    }

    def checkNone(latticeName: String, key: List[AnyRef]): Unit = {
      withClue(s"interpreted value $latticeName($key):") {
        val lattice = interpreted.getLattice(latticeName).toMap
        assertResult(None)(lattice.get(key))
      }
      withClue(s"compiled value $latticeName($key):") {
        val lattice = compiled.getLattice(latticeName).toMap
        assertResult(None)(lattice.get(key))
      }
    }

    def checkSuccess(): Unit = {
      assert(interpretedFlix.solve().isSuccess)
      assert(compiledFlix.solve().isSuccess)
    }

  }

  /////////////////////////////////////////////////////////////////////////////
  // Domains                                                                 //
  /////////////////////////////////////////////////////////////////////////////

  test("Belnap.flix") {
    val input =
      """namespace Belnap {
        |    let Belnap<> = (Belnap.Bot, Belnap.Top, leq, lub, glb)
        |    lat A(k: Int, v: Belnap)
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
        |    A(6, and(Belnap.True, Belnap.False)).
        |
        |    A(7, or(Belnap.True, Belnap.False)).
        |
        |    A(8, xor(Belnap.True, Belnap.False)).
        |}
      """.stripMargin

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addStr(input)
      .run()

    val Tru = Value.mkTag("True", Value.Unit)
    val Fls = Value.mkTag("False", Value.Unit)
    val Top = Value.mkTag("Top", Value.Unit)

    t.checkValue(Tru, "Belnap/A", List(Value.mkInt32(1)))
    t.checkValue(Fls, "Belnap/A", List(Value.mkInt32(2)))
    t.checkValue(Top, "Belnap/A", List(Value.mkInt32(3)))
    t.checkNone("Belnap/A", List(Value.mkInt32(4)))
    t.checkValue(Tru, "Belnap/A", List(Value.mkInt32(5)))
    t.checkValue(Fls, "Belnap/A", List(Value.mkInt32(6)))
    t.checkValue(Tru, "Belnap/A", List(Value.mkInt32(7)))
    t.checkValue(Tru, "Belnap/A", List(Value.mkInt32(8)))
  }

  test("Constant.flix") {
    val input =
      """namespace Domain.Constant {
        |    let Constant<> = (Constant.Bot, Constant.Top, leq, lub, glb)
        |    lat A(k: Int, v: Constant)
        |
        |    A(0, Cst(0)).
        |    A(1, Cst(1)).
        |    A(2, Cst(2)).
        |
        |    A(3, x) :- A(0, x).
        |    A(3, x) :- A(1, x).
        |    A(3, x) :- A(2, x).
        |
        |    A(4, x) :- A(0, x), A(1, x), A(2, x).
        |
        |    A(5, plus(x, y))  :- A(0, x), A(2, y).
        |    A(6, times(x, y)) :- A(1, x), A(2, y).
        |}
      """.stripMargin

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Constant.flix")
      .addStr(input)
      .run()

    val Zer = Value.mkTag("Cst", Value.mkInt32(0))
    val One = Value.mkTag("Cst", Value.mkInt32(1))
    val Two = Value.mkTag("Cst", Value.mkInt32(2))
    val Top = Value.mkTag("Top", Value.Unit)

    t.checkValue(Zer, "Domain.Constant/A", List(Value.mkInt32(0)))
    t.checkValue(One, "Domain.Constant/A", List(Value.mkInt32(1)))
    t.checkValue(Two, "Domain.Constant/A", List(Value.mkInt32(2)))
    t.checkValue(Top, "Domain.Constant/A", List(Value.mkInt32(3)))
    t.checkNone("Domain.Constant/A", List(Value.mkInt32(4)))
    t.checkValue(Two, "Domain.Constant/A", List(Value.mkInt32(5)))
    t.checkValue(Two, "Domain.Constant/A", List(Value.mkInt32(6)))
  }

  test("ConstantSign.flix") {
    val input =
      """namespace Domain.ConstantSign {
        |    let ConstantSign<> = (Bot, Top, leq, lub, glb)
        |    lat A(k: Int, v: ConstantSign)
        |
        |    A(1, Cst(-1ii)).
        |    A(2, Cst(0ii)).
        |    A(3, Cst(1ii)).
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
        |    A(9, times(x, y)) :- A(1, x), A(1, y). // 9 -> 1
        |}
      """.stripMargin

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/ConstantSign.flix")
      .addStr(input)
      .run()

    val Zer = Value.mkTag("Cst", Value.mkBigInt(0))
    val One = Value.mkTag("Cst", Value.mkBigInt(1))
    val Pos = Value.mkTag("Pos", Value.Unit)
    val Top = Value.mkTag("Top", Value.Unit)

    t.checkValue(Zer, "Domain.ConstantSign/A", List(Value.mkInt32(2)))
    t.checkValue(One, "Domain.ConstantSign/A", List(Value.mkInt32(3)))
    t.checkValue(Top, "Domain.ConstantSign/A", List(Value.mkInt32(4)))
    t.checkValue(Top, "Domain.ConstantSign/A", List(Value.mkInt32(4)))
    t.checkValue(Pos, "Domain.ConstantSign/A", List(Value.mkInt32(5)))
    t.checkNone("Domain.ConstantSign/A", List(Value.mkInt32(6)))
    t.checkNone("Domain.ConstantSign/A", List(Value.mkInt32(7)))
    t.checkValue(Pos, "Domain.ConstantSign/A", List(Value.mkInt32(8)))
    t.checkValue(One, "Domain.ConstantSign/A", List(Value.mkInt32(9)))
  }

  test("ConstantParity.flix") {
    val input = ""

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/ConstantParity.flix")
      .addStr(input)
      .run()

    // TODO: Exercise lattice.
  }

  test("Mod3.flix") {
    val input = ""

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Mod3.flix")
      .addStr(input)
      .run()

    // TODO: Exercise lattice.
  }

  test("Parity.flix") {
    val input =
      """namespace Domain.Parity {
        |    let Parity<> = (Parity.Bot, Parity.Top, leq, lub, glb)
        |    lat A(k: Int, v: Parity)
        |
        |    A(1, Odd).
        |    A(2, Even).
        |
        |    A(3, Odd).
        |    A(3, Even).
        |
        |    A(4, x) :- A(1, x), A(2, x).
        |
        |    A(5, plus(Odd, Even)).
        |
        |    A(6, plus(Odd, Odd)).
        |
        |    A(7, times(Odd, Even)).
        |
        |    A(8, times(Odd, Odd)).
        |}
      """.stripMargin

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Parity.flix")
      .addStr(input)
      .run()

    val Odd = Value.mkTag("Odd", Value.Unit)
    val Evn = Value.mkTag("Even", Value.Unit)
    val Top = Value.mkTag("Top", Value.Unit)

    t.checkValue(Odd, "Domain.Parity/A", List(Value.mkInt32(1)))
    t.checkValue(Evn, "Domain.Parity/A", List(Value.mkInt32(2)))
    t.checkValue(Top, "Domain.Parity/A", List(Value.mkInt32(3)))
    t.checkNone("Domain.Parity/A", List(Value.mkInt32(4)))
    t.checkValue(Odd, "Domain.Parity/A", List(Value.mkInt32(5)))
    t.checkValue(Evn, "Domain.Parity/A", List(Value.mkInt32(6)))
    t.checkValue(Evn, "Domain.Parity/A", List(Value.mkInt32(7)))
    t.checkValue(Odd, "Domain.Parity/A", List(Value.mkInt32(8)))
  }

  test("ParitySign.flix") {
    val input = ""

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/ParitySign.flix")
      .addStr(input)
      .run()

    // TODO: Exercise lattice.
  }

  test("PrefixSuffix.flix") {
    val input = ""

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/PrefixSuffix.flix")
      .addStr(input)
      .run()

    // TODO: Exercise lattice.
  }

  test("Sign.flix") {
    val input = ""

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Sign.flix")
      .addStr(input)
      .run()

    // TODO: Exercise lattice.
  }

  test("StrictSign.flix") {
    val input =
      """namespace Domain.StrictSign {
        |    let Sign<> = (Sign.Bot, Sign.Top, leq, lub, glb)
        |    lat A(k: Int, v: Sign)
        |
        |    A(1, Neg).
        |    A(2, Zer).
        |    A(3, Pos).
        |
        |    A(4, Neg).
        |    A(4, Zer).
        |    A(4, Pos).
        |
        |    A(5, x) :- A(1, x), A(2, x), A(3, x).
        |
        |    A(6, plus(Zer, Pos)).
        |    A(7, plus(Neg, Pos)).
        |
        |    A(8, times(Zer, Pos)).
        |    A(9, times(Neg, Neg)).
        |}
      """.stripMargin

    val t = new Tester()
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/StrictSign.flix")
      .addStr(input)
      .run()

    val Neg = Value.mkTag("Neg", Value.Unit)
    val Zer = Value.mkTag("Zer", Value.Unit)
    val Pos = Value.mkTag("Pos", Value.Unit)
    val Top = Value.mkTag("Top", Value.Unit)

    t.checkValue(Neg, "Domain.StrictSign/A", List(Value.mkInt32(1)))
    t.checkValue(Zer, "Domain.StrictSign/A", List(Value.mkInt32(2)))
    t.checkValue(Pos, "Domain.StrictSign/A", List(Value.mkInt32(3)))
    t.checkValue(Top, "Domain.StrictSign/A", List(Value.mkInt32(4)))
    t.checkNone("Domain.StrictSign/A", List(Value.mkInt32(5)))
    t.checkValue(Pos, "Domain.StrictSign/A", List(Value.mkInt32(6)))
    t.checkValue(Top, "Domain.StrictSign/A", List(Value.mkInt32(7)))
    t.checkValue(Zer, "Domain.StrictSign/A", List(Value.mkInt32(8)))
    t.checkValue(Pos, "Domain.StrictSign/A", List(Value.mkInt32(9)))
  }

  test("IFDS.flix") {
    val t = new Tester()
      .addPath("./examples/analysis/IFDS.flix")
      .run()
    t.checkSuccess()
  }

  test("IDE.flix") {
    val t = new Tester()
      .addPath("./examples/analysis/IDE.flix")
      .run()
    t.checkSuccess()
  }

  test("SUOpt.flix") {
    val t = new Tester()
      .addPath("./examples/analysis/SUopt.flix")
      .run()
    t.checkSuccess()
  }

}
