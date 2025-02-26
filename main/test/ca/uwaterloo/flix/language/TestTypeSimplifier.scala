package ca.uwaterloo.flix.language

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestTypeSimplifier extends AnyFunSuite with TestUtils {

  test("RunWithSimplifier") {
    val input = "def main(): Bool = true"
    val result = compile(input, Options.TestWithLibAll.copy(xUseSurfaceSimplifier = true))
    expectSuccess(result)
  }

}
