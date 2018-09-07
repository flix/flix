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
import ca.uwaterloo.flix.util.{Evaluation, Options}
import org.scalatest.FunSuite

class TestExamplesOld extends FunSuite {

  // TODO: Remove class.

  val DefaultOptions = Options.DefaultTest.copy(core = false, evaluation = Evaluation.Interpreted)

  test("Constant.flix") {
    val input =
      """namespace Domain/Constant {
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

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Constant.flix")
      .addStr(input)
      .solve().get
  }

  test("ConstantSign.flix") {
    val input =
      """namespace Domain/ConstantSign {
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

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/ConstantSign.flix")
      .addStr(input)
      .solve().get
  }

  test("ConstantParity.flix") {
    val input = ""

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/ConstantParity.flix")
      .addStr(input)
      .solve().get

    // TODO: Exercise lattice.
  }

  test("Mod3.flix") {
    val input = ""

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Mod3.flix")
      .addStr(input)
      .solve().get

    // TODO: Exercise lattice.
  }

  test("Parity.flix") {
    val input =
      """namespace Domain/Parity {
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

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Parity.flix")
      .addStr(input)
      .solve().get
  }

  test("ParitySign.flix") {
    val input = ""

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/ParitySign.flix")
      .addStr(input)
      .solve().get

    // TODO: Exercise lattice.
  }

  test("PrefixSuffix.flix") {
    val input = ""

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/PrefixSuffix.flix")
      .addStr(input)
      .solve().get

    // TODO: Exercise lattice.
  }

  test("Sign.flix") {
    val input = ""

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/Sign.flix")
      .addStr(input)
      .solve().get

    // TODO: Exercise lattice.
  }

  test("StrictSign.flix") {
    val input =
      """namespace Domain/StrictSign {
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

    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/domains/Belnap.flix")
      .addPath("./examples/domains/StrictSign.flix")
      .addStr(input)
      .solve().get
  }

  test("IFDS.flix") {
    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/analysis/IFDS.flix")
      .solve().get
  }

  test("IDE.flix") {
    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/analysis/IDE.flix")
      .solve().get
  }

  test("SUOpt.flix") {
    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/analysis/SUopt.flix")
      .solve().get
  }

  test("FloydWarshall.flix") {
    new Flix()
      .setOptions(DefaultOptions)
      .addPath("./examples/misc/FloydWarshall.flix")
      .solve().get
  }

}
