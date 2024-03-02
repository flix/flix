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
import ca.uwaterloo.flix.language.phase.unification.FastBoolUnification.{Term, solveAll, verify}
import org.scalatest.funsuite.AnyFunSuite

class TestFastBoolUnification extends AnyFunSuite with TestUtils {

  import Term._

  private implicit val defaultLoc: SourceLocation = SourceLocation.Unknown

  test("MutDeque.sameElements") {
    val l = List(
      (Var(876) & Var(877)) ~ Var(90798),
      Var(90798) ~ (Var(90801) & Var(90804) & Var(90807) & Var(90820) & Var(90823) & Var(90832) & Var(90841) & Var(90844)),
      Var(90801) ~ Var(134687),
      Var(90804) ~ Var(134689),
      Var(90807) ~ True,
      Var(90820) ~ Var(134695),
      Var(90823) ~ Var(134697),
      Var(90826) ~ Var(134703),
      Var(90828) ~ Var(134705),
      Var(90830) ~ Var(134707),
      Var(90832) ~ (Var(134701) & Var(134699) & Var(90826) & Var(90828) & Var(90830)),
      Var(90835) ~ Var(134712),
      Var(90837) ~ Var(134714),
      Var(90839) ~ Var(134716),
      Var(90841) ~ (Var(134710) & Var(134708) & Var(90835) & Var(90837) & Var(90839)),
      Var(90844) ~ (Var(134718) & Var(134719))
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Fixpoint.Ast.Datalog.predSymsOf$29898") {
    val l = List(
      True ~ (Var(97799) & Var(97816) & Var(97819) & Var(97847) & Var(97859)),
      Var(97787) ~ Var(107197),
      Var(97790) ~ True,
      Var(97792) ~ Var(107195),
      Var(97794) ~ (Var(107192) & Var(97792)),
      Var(97797) ~ True,
      Var(97799) ~ (Var(107189) & Var(97794)),
      Var(97804) ~ Var(107211),
      Var(97807) ~ True,
      Var(97809) ~ Var(107209),
      Var(97811) ~ (Var(107206) & Var(97809)),
      Var(97814) ~ True,
      Var(97816) ~ (Var(107203) & Var(97811)),
      Var(97819) ~ True,
      Var(97827) ~ True,
      Var(97830) ~ True,
      Var(97832) ~ (Var(107224) & Var(97827)),
      Var(97835) ~ Var(107232),
      Var(97838) ~ True,
      Var(97840) ~ Var(107230),
      Var(97842) ~ (Var(107221) & Var(97832) & Var(97840)),
      Var(97845) ~ True,
      Var(97847) ~ (Var(107218) & Var(97842)),
      Var(97854) ~ True,
      Var(97857) ~ True,
      Var(97859) ~ (Var(97854) & Var(97857))
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Iterator.toArray") {
    val l = List(
      ((Cst(1500) & Cst(1501)) & Cst(1498)) ~ Var(78914),
      Var(78914) ~ (Var(78917) & (Var(78923) & Var(78926))),
      Var(78917) ~ Var(127244),
      Var(78921) ~ Var(127251),
      Var(78923) ~ ((Var(127248) & Var(127247)) & Var(127249)),
      Var(78926) ~ (Var(127254) & Var(127252))
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Files.append") {
    val l = List(
      Cst(1794221043) ~ ((Var(55040) & (Var(55042) & (Var(55046) & (Var(55050) & (Var(55058) & (Var(55060) & (Var(55062) & (Var(55066) & Var(55075))))))))) & Var(55078)),
      Var(55078) ~ Var(112431),
      Var(55040) ~ Cst(1794221043),
      Var(55042) ~ Var(112433),
      Var(55044) ~ Var(112437),
      Var(55046) ~ (Var(112435) & Var(55044)),
      Var(55048) ~ True,
      Var(55050) ~ (Var(112439) & Var(55048)),
      Var(55052) ~ Var(112443),
      Var(55055) ~ True,
      Var(55058) ~ (Var(112441) & (Var(55052) & Var(55055))),
      Var(55060) ~ Var(112446),
      Var(55062) ~ Var(112448),
      Var(55066) ~ True,
      Var(55075) ~ Var(112453)
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Iterator.next") {
    val l = List(
      (Cst(1435) & Cst(1436)) ~ Var(55261),
      Var(55251) ~ Var(112576),
      Var(55257) ~ Var(112582),
      Var(55261) ~ Var(112585)
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Boxable.lift1") {
    val l = List(
      Var(56474) ~ True,
      Var(56476) ~ (Var(113308) & Var(56474)),
      Var(56478) ~ Var(56476)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

}
