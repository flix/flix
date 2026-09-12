/*
 * Copyright 2023 Matthew Lutze
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class BenchmarkSuite extends FlixSuite(incremental = true) {

  private implicit val TestOptions: Options = Options.TestWithLibAll

  mkTestDirCollected("main/src/resources/benchmark", name = "BenchmarkSuite.flix")

}
