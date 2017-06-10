/*
 * Copyright 2015-2016 Jason Mittertreiner
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

package ca.uwaterloo.flix.language.phase

  import ca.uwaterloo.flix.TestUtils
  import ca.uwaterloo.flix.api.Flix
  import ca.uwaterloo.flix.runtime.Model
  import ca.uwaterloo.flix.util.Options
  import org.scalatest.FunSuite

  class TestUncurrier extends FunSuite with TestUtils {

    /**
      * Runs Flix on the given input string `s`.
      */
    def run(s: String, core: Boolean = true): Model = {
      new Flix().setOptions(Options.DefaultTest.copy(core = false)).addStr(s).solve().get
    }

    test("Simple.01") {
      val input =
        """def add(x: Int): Int -> Int -> Int = {
          |    y -> (z -> x + y + z)
          |}
          |
          |def a(): Int = ((add(3))(4))(5)
        """.stripMargin
      run(input)
    }

    test("Simple.02") {
      val input =
        """def add(x: Int): Int -> Int -> Int = {
          |    y -> (z -> x + y + z)
          |}
          |
          |def a(): Int = ((add(3))(4))(5)
          |def b(): Int -> Int = (add(3))(4)
          |def c(): Int -> Int -> Int = add(3)
        """.stripMargin
      run(input)
    }

    test("Multi.01") {
      val input =
        """def add(x: Int): Int -> Int -> Int = {
          |    y -> (z -> x + y + z)
          |}
          |def sub(x: Int): Int -> Int = {
          |    y -> (x - y)
          |}
          |
          |def a(): Int = ((add((sub(3))(4)))(4))(5)
        """.stripMargin
      run(input)
    }

    test("Map.01") {
      val input =
        """def add(x: Int32): Int32 -> Int32 = {
          |    y -> x + y
          |}
          |
          |def lst(): List[Int32] = 1 :: 2 :: 3 :: 4 :: Nil
          |
          |
          |def a(): List[Int32]= List.map(add(2), lst())
        """.stripMargin
      run(input)
    }

    test("Map.02") {
      val input =
        """def add(x: Int32): Int32 -> Int32 -> Int32 = {
          |    y -> (z -> x + y + z)
          |}
          |
          |def lst(): List[Int32] = 1 :: 2 :: 3 :: 4 :: Nil
          |
          |
          |def a(): List[Int32]= List.map((add(2))(3), lst())
        """.stripMargin
      run(input)
    }

    test("ReturnFunc.01") {
      val input =
        """def id(x: (Int32) -> Int32): (Int32) -> Int32 = x
          |
          |def a(): Int32 =  (id(x -> x))(3)
        """.stripMargin
      run(input)
    }

    test("Lambdas.01") {
      val input =
        """def add(i: Int32): Int32 =  {
          |    ((((w -> x -> y -> z -> w + x + y + z + i)(2))(3))(4))(5)
          |}
          |
          |def a(): Int32 =  add(3)
        """.stripMargin
      run(input)
    }

    test("Lambdas.02") {
      val input =
        """def add(i: Int32): Int32 -> Int32 =  {
          |    (((w -> x -> y -> z -> w + x + y + z + i)(2))(3))(4)
          |}
          |
          |def a(): Int32 =  (add(3))(2)
        """.stripMargin
      run(input)
    }

    test("Lambdas.03") {
      val input =
        """def add(i: Int32): Int32 -> Int32 -> Int32 -> Int32 -> Int32 =  {
          |    w -> x -> y -> z -> w + x + y + z + i
          |}
          |
          |def a(): Int32 =  ((((add(3))(2))(3))(3)(2))
        """.stripMargin
      run(input)
    }

    test("ZeroArgs.01") {
      val input =
        """def add4(): Int32 -> Int32 = x -> 4
          |
          |def add(i: Int32): Int32 =  {
          |    (add4())(i)
          |}
          |
          |def a(): Int32 =  add(4)
        """.stripMargin
      run(input)
    }

    test("NestedDef.01") {
      val input =
        """def add(i: Int32): Int32 = {
          |    let nestedAdd = y -> x -> i;
          |    (nestedAdd(3))(3)
          |}
          |
          |def a(): Int32 =  add(4)
        """.stripMargin
      run(input)
    }

  }
