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

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.language.phase.unification.FastBoolUnification.{Term, verify}
import org.scalatest.funsuite.AnyFunSuite

class TestFastBoolUnification extends AnyFunSuite with TestUtils {

  import Term._

  private implicit val defaultLoc: SourceLocation = SourceLocation.Unknown

  test("Boxable.lift1") {
    val l = List(
      Var(56474) ~ True,
      Var(56476) ~ (Var(113308) & Var(56474)),
      Var(56478) ~ Var(56476)
    )

    val s = FastBoolUnification.solveAll(l).get
    assertResult(expected = true)(verify(s, l))
  }

}
