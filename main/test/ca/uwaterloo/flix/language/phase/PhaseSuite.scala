/*
 * Copyright 2016 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.language.phase.unification.{TestBdd, TestBoolFormulaTable, TestQMCtoBoolFormula, TestUnification}
import ca.uwaterloo.flix.language.phase.TestDeriver
import org.scalatest.Suites

class PhaseSuite extends Suites(
  // phases
  new TestDeriver,
  new TestEntryPoint,
  new TestInstances,
  new TestKinder,
  new TestNamer,
  new TestParser,
  new TestPatExhaustiveness,
  new TestRedundancy,
  new TestRegions,
  new TestResolver,
  new TestSafety,
  new TestStratifier,
  new TestTyper,
  new TestWeeder,

  // helpers
  new TestBdd,
  new TestBoolFormulaTable,
  new TestQMCtoBoolFormula,
  new TestIncremental,
  new TestUnification
)
