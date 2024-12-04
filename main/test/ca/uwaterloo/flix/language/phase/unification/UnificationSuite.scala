/*
 *  Copyright 2024 Magnus Madsen
 *
 *  Licensed under the Apache License, Version 2.0 (the "License");
 *  you may not use this file except in compliance with the License.
 *  You may obtain a copy of the License at
 *
 *  http://www.apache.org/licenses/LICENSE-2.0
 *
 *  Unless required by applicable law or agreed to in writing, software
 *  distributed under the License is distributed on an "AS IS" BASIS,
 *  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 *  See the License for the specific language governing permissions and
 *  limitations under the License.
 */
package ca.uwaterloo.flix.language.phase.unification

import ca.uwaterloo.flix.language.phase.TestIncremental
import ca.uwaterloo.flix.language.phase.unification.set.TestSetUnification
import ca.uwaterloo.flix.language.phase.unification.zhegalkin.TestZhegalkin
import org.scalatest.Suites

class UnificationSuite extends Suites(
  new TestFastBoolUnification,
  new TestIncremental,
  new TestSetUnification,
  new TestZhegalkin
)
