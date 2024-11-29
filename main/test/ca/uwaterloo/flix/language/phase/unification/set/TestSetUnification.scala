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
import ca.uwaterloo.flix.language.phase.unification.set.EquationGenerator.{mkEq, xorSelfAugmented}
import ca.uwaterloo.flix.language.phase.unification.set.SetFormulaGenerator.{Options, generate}
import ca.uwaterloo.flix.language.phase.unification.set.SetUnification.{SolverListener, solve, stateString, verifySubst}
import ca.uwaterloo.flix.util.Result
import org.scalatest.funsuite.AnyFunSuite

import scala.util.Random

class TestSetUnification extends AnyFunSuite with TestUtils {

  implicit val genOpts: Options = Options.default
  implicit val solveOpts: SetUnification.Options = SetUnification.Options.default
  implicit val listener: SolverListener = SolverListener.DoNothing

  test("TestSetUnification.EqSelf") {
    val seed = System.currentTimeMillis()
    implicit val r: Random = new Random(seed)
    // Always print seed to ensure the ability to reproduce failing tests.
    println(s"Testing with seed $seed")

    for (size <- 0 until 500) {
      val f = generate(size, maxDepth = -1)
      val inputEq = mkEq(f, f)
      val input = List(inputEq)
      val (eqs, subst) = solve(input)
      assert(eqs.isEmpty, s"\nCould not solve equation (seed: $seed): $inputEq\n${stateString(eqs, subst)}")
      verifySubst(input, subst) match {
        case Result.Err(msg) =>
          assert(false, s"\nFound substitution is not a solution (seed: $seed):\n$msg\n${stateString(input, subst)}")
        case Result.Ok(()) => ()
      }
    }
  }

  test("TestSetUnification.NotEqComplementSelf") {
    val seed = System.currentTimeMillis()
    implicit val r: Random = new Random(seed)
    // Always print seed to ensure the ability to reproduce failing tests.
    println(s"Testing with seed $seed")

    for (size <- 0 until 500) {
      val f = generate(size, maxDepth = -1)
      val inputEq = mkEq(f, SetFormula.mkCompl(f))
      val input = List(inputEq)
      val (eqs, subst) = solve(input)
      assert(eqs.nonEmpty, s"\nCould not solve equation (seed: $seed): $inputEq\n${stateString(eqs, subst)}")
    }
  }

  test("TestSetUnification.XorSelfAugmented") {
    val seed = System.currentTimeMillis()
    implicit val r: Random = new Random(seed)
    // Always print seed to ensure the ability to reproduce failing tests.
    println(s"Testing with seed $seed")

    for (size <- 0 until 10) {
      val f = generate(size, maxDepth = -1)
      val augment = generate(size = 3, maxDepth = -1)
      val inputEq = xorSelfAugmented(f, augment)
      val input = List(inputEq)
      val (eqs, subst) = solve(input)
      assert(eqs.isEmpty, s"\nCould unexpectedly solve equation (seed: $seed): $inputEq\n${stateString(eqs, subst)}")
      verifySubst(input, subst) match {
        case Result.Err(msg) =>
          assert(false, s"\nFound substitution is not a solution (seed: $seed):\n$msg\n${stateString(input, subst)}")
        case Result.Ok(()) => ()
      }
    }
  }

}
