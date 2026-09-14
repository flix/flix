/*
 * Copyright 2023 Sam Ezeh
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.DerivationError
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestDeriver extends AnyFunSuite with TestUtils {

  test("DerivationError.EmptyEnum.Eq") {
    val compiled = check("enum E with Eq", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("DerivationError.EmptyEnum.Order") {
    val compiled = check("enum E with Eq, Order", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("DerivationError.EmptyEnum.ToString") {
    val compiled = check("enum E with ToString", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("DerivationError.EmptyEnum.Hash") {
    val compiled = check("enum E with Hash", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("DerivationError.EmptyEnum.Coerce") {
    val compiled = check("enum E with Coerce", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("DerivationError.MultiEnum.Coerce") {
    val compiled = check("enum E with Coerce", Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivationForEmptyEnum](compiled)
  }

  test("IllegalDerivation.01") {
    val input =
      """
        |trait C[a]
        |
        |enum E with C {
        |    case E
        |}
        |""".stripMargin
    val result = check(input, Options.TestWithLibMin)
    expectError[DerivationError.IllegalDerivation](result)
  }

}

