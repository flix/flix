/*
 * Copyright 2026 Din Jakupi
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.fmt

import org.scalatest.Ignore

/**
  * Stability tests for the Flix code formatter.
  *
  * Stability is an *aesthetic* property: `forall l in stdlib ++ examples. f(p(l)) = l`.
  *
  * The standard library and the `examples` programs are maintained in a
  * canonical formatted form. Formatting them must reproduce the same output.
  */
@Ignore
class TestFormatterStability extends TestFormatterCommon {

  /**
    * Stability: `forall l in stdlib ++ examples. f(p(l)) = l`.
    *
    * Formatting a file that is already in canonical form must reproduce it exactly.
    */
  private def checkStability(samples: List[Sample]): Unit = {
    for (sample <- samples) {
      val formatted = PrettyPrinter.format(sample.reparse(sample.content).tree)
      val isFixedPoint = formatted == sample.content
      assert(isFixedPoint,
        s"Standard library is not preserved by the formatter (f(p(l)) != l) " +
          s"for ${sample.path}:\n${firstDivergence(sample.content, formatted)}")
    }
  }

  test("PrettyPrinter: stability (examples)") {
    checkStability(ExampleSamples)
  }
  test("PrettyPrinter: stability (stdlib)") {
    checkStability(StdlibSamples)
  }
}
