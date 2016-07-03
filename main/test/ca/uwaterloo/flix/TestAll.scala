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

import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.runtime.{TestDeltaDebugger, TestBackend, TestSolver, TestValue}
import ca.uwaterloo.flix.util.TestValidation
import org.scalatest.{ParallelTestExecution, Suites}

// TODO: re-organize

// NB: Run with -P to run in parallel.
class TestAll extends Suites(
  new TestTypedAst,
  new TestParser,
  new TestResolver,
  new TestTyper,
  new TestWeeder,
  new TestBackend,
  new TestSolver,
  new TestValue,
  new TestValidation,
  new TestMain,
  new TestDeltaDebugger
  //new TestLibrary, // TODO
  // new TestExamples // TODO: temporarily removed while we figure out what to do with travis.
) with ParallelTestExecution {

}
