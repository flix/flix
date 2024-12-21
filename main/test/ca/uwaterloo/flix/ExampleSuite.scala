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
package ca.uwaterloo.flix

import ca.uwaterloo.flix.util.{FlixSuite, Options}

class ExampleSuite extends FlixSuite(incremental = true) {

  private implicit val TestOptions: Options = Options.TestWithLibAll

  mkTestDir("examples/concurrency-and-parallelism")
  mkTestDir("examples/effects-and-handlers")
  mkTestDir("examples/fixpoints")
  mkTestDir("examples/functional-style")
  mkTestDir("examples/imperative-style")

  mkTestDir("examples/interoperability/anonymous-classes")
  mkTestDir("examples/interoperability/calling-methods")
  mkTestDir("examples/interoperability/exceptions")
  mkTestDir("examples/interoperability/files")
  mkTestDir("examples/interoperability/swing")

  mkTestDir("examples/misc")
  mkTestDir("examples/misc/type-level-programming")

  mkTestDir("examples/io")
  mkTestDir("examples/records")
  mkTestDir("examples/structs")
  mkTestDir("examples/traits")

  mkTestDir("examples/larger-examples")
  mkTestDir("examples/larger-examples/datalog")
  mkTestDir("examples/larger-examples/program-analysis")
  mkTestDir("examples/larger-examples/program-analysis/domains")
  mkTestDir("examples/larger-examples/restrictable-variants")

  mkTestDir("examples/modules")

}
