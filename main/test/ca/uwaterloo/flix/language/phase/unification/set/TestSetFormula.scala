/*
 * Copyright 2024 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.phase.unification.set

import ca.uwaterloo.flix.TestUtils
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class TestSetFormula extends AnyFunSuite with TestUtils {

  test("TestSetFormula.Propagation") {
    val seed = System.currentTimeMillis()
    val r = new Random(seed)
    // Always print seed to ensure the ability to reproduce failing tests.
    println(s"Testing with seed $seed")
    for (i <- 0 until 500) {
      val opts = SetFormulaGenerator.Options(maxConnectiveWidth = 3, varDomSize = 3, cstDomSize = 3, elemDomSize = 3)
      val f = SetFormulaGenerator.generate(i, -1)(r, opts)
      val fProp = SetFormula.propagation(f)
      assert(SetFormula.isEquivalent(f, fProp), s"Formulas not equivalent (seed: $seed): $f ~ $fProp")
    }
  }

}
