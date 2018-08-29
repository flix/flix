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

package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{Evaluation, Options}
import org.scalatest.FunSuite

class TestSolver extends FunSuite {

  val opts = Options.DefaultTest.copy(evaluation = Evaluation.Interpreted) // TODO

  object Parity {

    val Definition =
      """
        |    enum Parity {
        |                  case Top,
        |        case Odd,          case Even,
        |                  case Bot
        |    }
        |
        |    def equ(e1: Parity, e2: Parity): Bool = e1 == e2
        |
        |    def leq(e1: Parity, e2: Parity): Bool = match (e1, e2) with {
        |        case (Parity.Bot, _)              => true
        |        case (Parity.Odd, Parity.Odd)     => true
        |        case (Parity.Even, Parity.Even)   => true
        |        case (_, Parity.Top)              => true
        |        case _                            => false
        |    }
        |
        |    def lub(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        |        case (Parity.Bot, _)              => e2
        |        case (_, Parity.Bot)              => e1
        |        case (Parity.Odd, Parity.Odd)     => Parity.Odd
        |        case (Parity.Even, Parity.Even)   => Parity.Even
        |        case _                            => Parity.Top
        |    }
        |
        |    def glb(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        |        case (Parity.Top, _) => e2
        |        case (_, Parity.Top) => e1
        |        case (Parity.Odd, Parity.Odd) => Parity.Odd
        |        case (Parity.Even, Parity.Even) => Parity.Even
        |        case _ => Parity.Bot
        |    }
        |
        |    def plus(e1: Parity, e2: Parity): Parity = match (e1, e2) with {
        |        case (_, Parity.Bot)              => Parity.Bot
        |        case (Parity.Bot, _)              => Parity.Bot
        |        case (Parity.Odd, Parity.Odd)     => Parity.Even
        |        case (Parity.Odd, Parity.Even)    => Parity.Odd
        |        case (Parity.Even, Parity.Odd)    => Parity.Odd
        |        case (Parity.Even, Parity.Even)   => Parity.Even
        |        case _                            => Parity.Top
        |    }
        |
        |    def isMaybeZero(e: Parity): Bool = match e with {
        |        case Parity.Bot    => false
        |        case Parity.Odd    => false
        |        case Parity.Even   => true
        |        case Parity.Top    => true
        |    }
        |
        |    let Parity<> = (Parity.Bot, Parity.Top, equ, leq, lub, glb)
      """.stripMargin

  }

  test("Cross01") {
    val s =
      """rel A(x: Int, y: Int)
        |
        |A(1, 2).
        |A(2, 3).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross02") {
    val s =
      """rel A(x: Int, y: Int)
        |
        |A(1, 2).
        |A(2, 3).
        |A(3, 4).
        |A(4, 5).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross03") {
    val s =
      """rel A(x: Int, y: Int)
        |
        |A(1, 2).
        |A(2, 3).
        |A(3, 1).
        |
        |A(x, z) :- A(x, y), A(y, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross04") {
    val s =
      """rel A(x: Int, y: Str, z: Int)
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(1, "b", 2).
        |
        |A(x, c, z) :- A(x, c, y), A(y, c, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross05") {
    val s =
      """rel A(x: Int, y: Str, z: Int)
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, "b", y) :- A(x, "a", y).
        |A(x, c, z) :- A(x, c, y), A(y, c, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross06") {
    val s =
      """rel A(x: Int, y: Str, z: Int)
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, c, y) :- A(y, c, x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross07") {
    val s =
      """rel A(x: Int, y: Str, z: Int)
        |
        |A(1, "a", 2).
        |A(2, "a", 3).
        |A(3, "b", 1).
        |
        |A(x, c, y) :- A(y, c, x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross08") {
    val s =
      """rel A(x: Int)
        |rel B(x: Int)
        |rel R(x: Int)
        |
        |A(1). A(2).
        |B(2). B(3).
        |
        |R(x) :- A(x), B(x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross09") {
    val s =
      """rel A(x: Int)
        |rel B(x: Int)
        |rel C(x: Int)
        |
        |rel R(x: Int)
        |
        |A(1). A(2). A(3).
        |B(2). B(3).
        |C(3).
        |
        |R(x) :- A(x), B(x), C(x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross10") {
    val s =
      """rel A(x: Int)
        |rel B(x: Int)
        |rel C(x: Int)
        |
        |rel R(x: Int)
        |
        |A(1). A(2). A(3).
        |B(2). B(3).
        |C(3).
        |
        |R(x) :- C(x), A(x), B(x), B(x), C(x), A(x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross11") {
    val s =
      """rel A(x: Int, y: Int)
        |rel B(x: Int, y: Int)
        |
        |rel R(x: Int, y: Int)
        |
        |A(1, 2). A(3, 4).
        |B(2, 3). B(4, 5).
        |
        |R(x, z) :- A(x, y), B(y, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross12") {
    val s =
      """rel A(x: Int, y: Int)
        |rel B(x: Int, y: Int)
        |rel C(x: Int, y: Int)
        |
        |rel R(x: Int, y: Int)
        |
        |A(1, 2). A(3, 4).
        |B(2, 3). B(4, 5).
        |C(3, 7).
        |
        |R(x, w) :- A(x, y), B(y, z), C(z, w).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross13") {
    val s =
      """rel A(x: Int, y: Int)
        |rel B(x: Int, y: Int)
        |rel C(x: Int, y: Int)
        |
        |A(1, 2). A(7, 1).
        |B(2, 3). B(3, 5).
        |C(3, 4). C(5, 6).
        |
        |C(x, z) :- A(x, y), B(y, z).
        |A(x, z) :- C(x, y), B(y, z).
        |B(x, z) :- A(x, y), C(y, z).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Cross14") {
    val s =
      """rel A(x: Int)
        |rel B(y: Int)
        |rel C(z: Int)
        |
        |rel R(x: Int)
        |
        |A(1). A(2).
        |// B empty
        |C(1). C(2).
        |
        |R(x) :- A(x), B(x), C(x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Wildcard01") {
    val s =
      """rel A(x: Int, y: Int)
        |rel B(x: Int, y: Int)
        |rel C(x: Int, y: Int)
        |
        |A(1, 2).
        |A(3, 4).
        |
        |B(5, 6).
        |B(7, 8).
        |
        |C(x, y) :- A(x, _), B(_, y).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("Lattice01") {
    val s =
      """lat A(x: Int, v: Parity)
        |
        |A(1, Parity.Odd).
        |A(2, Parity.Even).
        |A(3, Parity.Top).
        |
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(Parity.Definition).addStr(s).solve().get
  }

  test("Lattice02") {
    val s =
      """lat A(x: Int, v: Parity)
        |
        |A(1, Parity.Odd).
        |A(1, Parity.Even).
        |
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(Parity.Definition).addStr(s).solve().get
  }

  test("Lattice03") {
    val s =
      """lat A(x: Int, v: Parity)
        |
        |A(1, Parity.Odd).
        |A(2, Parity.Even).
        |
        |A(3, x) :- A(1,x).
        |A(3, x) :- A(2,x).
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(Parity.Definition).addStr(s).solve().get
  }

  test("NotEqual01") {
    val s =
      """rel A(x: Int)
        |rel B(x: Int, y: Int)
        |
        |A(1). A(2). A(3). A(4).
        |
        |def neq(x: Int, y: Int): Bool = x != y
        |
        |B(x, y) :- A(x), A(y), x != y.
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("NotEqual02") {
    val s =
      """rel A(x: Int, y: Int)
        |rel B(x: Int, y: Int)
        |
        |A(1, 2).
        |A(2, 2).
        |
        |def neq(x: Int, y: Int): Bool = x != y
        |
        |B(x, y) :- A(x, y), x != y.
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

  test("NotEqual03") {
    val s =
      """rel A(x: Int, y: Int)
        |rel B(x: Int, y: Int)
        |
        |A(1, 2).
        |A(2, 1).
        |A(2, 3).
        |
        |def neq(x: Int, y: Int): Bool = x != y
        |
        |B(x, z) :- A(x, y), A(y, z), x != z.
      """.stripMargin

    val model = new Flix().setOptions(opts).addStr(s).solve().get
  }

}
