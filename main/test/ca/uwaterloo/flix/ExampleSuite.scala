/*
 * Copyright 2022 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}
import org.scalatest.DoNotDiscover

// Excluded from the main `flix.test` run; executed by the dedicated `flix.testExamples` task.
@DoNotDiscover
class ExampleSuite extends FlixSuite(incremental = true) {

  private implicit val TestOptions: Options = Options.TestWithLibAll

  mkTestDir("examples/concurrency-and-parallelism")

  mkTestDir("examples/effects-and-handlers")
  mkTestDir("examples/effects-and-handlers/advanced")
  mkTestDir("examples/effects-and-handlers/assert")
  mkTestDir("examples/effects-and-handlers/console")
  mkTestDir("examples/effects-and-handlers/dns")
  mkTestDir("examples/effects-and-handlers/env")
  mkTestDir("examples/effects-and-handlers/fs")
  mkTestDir("examples/effects-and-handlers/http")
  mkTestDir("examples/effects-and-handlers/process")
  mkTestDir("examples/effects-and-handlers/sleep")
  mkTestDir("examples/effects-and-handlers/tcp")

  mkTestDir("examples/datalog")

  mkTestDir("examples/functional-style")

  mkTestDir("examples/imperative-style")

  mkTestDir("examples/interoperability/anonymous-classes")
  mkTestDir("examples/interoperability/calling-methods")
  mkTestDir("examples/interoperability/exceptions")
  mkTestDir("examples/interoperability/files")
  mkTestDir("examples/interoperability/swing")

  mkTestDir("examples/tail-recursion-and-termination")

  mkTestDir("examples/unsorted")
  mkTestDir("examples/unsorted/restrictable-variants")
  mkTestDir("examples/unsorted/static-analysis")
  mkTestDir("examples/unsorted/static-analysis/domains")
  mkTestDir("examples/unsorted/type-level-programming")

  mkTestDir("examples/modules")

  mkTestDir("examples/records")

  mkTestDir("examples/structs")

  mkTestDir("examples/traits")

}
