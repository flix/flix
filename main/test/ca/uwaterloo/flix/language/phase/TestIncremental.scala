/*
 * Copyright 2022 Magnus Madsen
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.Options
import org.scalatest.{BeforeAndAfterAll, FunSuite}

class TestIncremental extends FunSuite with BeforeAndAfterAll {

  //
  // Note: This class deliberately uses the *same* Flix instance.
  //
  // This is because we want to test incremental compilation by compiling in the same instance.
  //

  private val FileA = "FileA.flix"
  private val FileB = "FileB.flix"
  private val FileC = "FileC.flix"
  private val Flix = new Flix()
  private val Opts = Options.Default.copy(incremental = true)

  override protected def beforeAll(): Unit = {
    Flix.setOptions(Opts)
    Flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Bool): Bool = not x
         |
         |""".stripMargin)
    Flix.addSourceCode(FileB,
      s"""
         |def main(_args: Array[String]): Int32 & Impure =
         |    println(f(true));
         |    0
         |""".stripMargin)
    Flix.addSourceCode(FileC,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: Bool, y: a, z: a): a = if (f(x) == x) y else z
         |}
         |""".stripMargin)
    Flix.compile().get
  }

  test("Incremental.01") {
    Flix.addSourceCode(FileA,
      s"""
         |pub def f(x: Int32): Int32 = x + 1i32
         |
         |""".stripMargin)
    Flix.addSourceCode(FileB,
      s"""
         |def main(_args: Array[String]): Int32 & Impure =
         |    println(f(123));
         |    0
         |""".stripMargin)
    Flix.addSourceCode(FileC,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: Int32, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cg(x: a): a
         |}
         |""".stripMargin)
    Flix.compile().get
  }

  test("Incremental.02") {
    Flix.addSourceCode(FileA,
      s"""
         |pub def f(x: String): String = String.toUpperCase(x)
         |""".stripMargin)
    Flix.addSourceCode(FileB,
      s"""
         |def main(_args: Array[String]): Int32 & Impure =
         |    println(f("Hello World"));
         |    0
         |""".stripMargin)
    Flix.addSourceCode(FileC,
      s"""
         |pub lawless class C[a] {
         |    pub def cf(x: String, y: a, z: a): a = if (f(x) == x) y else z
         |    pub def cg(x: a): a
         |    pub def ch(x: a): a
         |}
         |""".stripMargin)
    Flix.compile().get
  }

}
