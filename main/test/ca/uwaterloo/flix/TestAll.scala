/*
 * Copyright 2015-2016 Magnus Madsen
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

import ca.uwaterloo.flix.api.ApiSuite
import ca.uwaterloo.flix.language.LanguageSuite
import ca.uwaterloo.flix.library.LibrarySuite
import ca.uwaterloo.flix.tools.ToolsSuite
import ca.uwaterloo.flix.util.UtilSuite
import flix.CompilerSuite
import flix.experimental.ExperimentalSuite
import flix.fuzzers.FuzzerSuite
import org.scalatest.Suites

class TestAll extends Suites(
  new ApiSuite,
  new BenchmarkSuite,
  new CompilerSuite,
  new FuzzerSuite,
  new ExampleSuite,
  new ExperimentalSuite,
  new LanguageSuite,
  new LibrarySuite,
  new TestMain,
  new ToolsSuite,
  new UtilSuite,
)
