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

  test("Fixpoint.Interpreter.evalTerm") {
    val l = List(
      Cst(442) ~ (Var(69984) & (Var(69992) & (Var(70006) & (Var(70010) & ((Var(70016) & Var(70018)) & ((Var(70025) & (Var(70028) & Var(70032))) & ((Var(70040) & (Var(70043) & (Var(70046) & Var(70052)))) & ((Var(70061) & (Var(70064) & (Var(70067) & (Var(70070) & Var(70078))))) & ((Var(70088) & (Var(70091) & (Var(70094) & (Var(70097) & (Var(70100) & Var(70110)))))) & (Var(70126) & Var(70135))))))))))),
      Var(69982) ~ Var(121740),
      Var(69984) ~ Var(69982),
      Var(69992) ~ Var(121742),
      Var(69999) ~ Var(121748),
      Var(70001) ~ (Var(121746) & Var(69999)),
      Var(70004) ~ Var(121750),
      Var(70006) ~ (Var(121744) & (Var(70001) & Var(70004))),
      Var(70010) ~ Var(121752),
      Var(70016) ~ Var(121754),
      Var(70018) ~ Var(121756),
      Var(70025) ~ Var(121758),
      Var(70028) ~ Var(121760),
      Var(70030) ~ Var(121764),
      Var(70032) ~ (Var(121762) & Var(70030)),
      Var(70040) ~ Var(121766),
      Var(70043) ~ Var(121768),
      Var(70046) ~ Var(121770),
      Var(70048) ~ Var(121776),
      Var(70050) ~ (Var(121774) & Var(70048)),
      Var(70052) ~ (Var(121772) & Var(70050)),
      Var(70061) ~ Var(121778),
      Var(70064) ~ Var(121780),
      Var(70067) ~ Var(121782),
      Var(70070) ~ Var(121784),
      Var(70072) ~ Var(121792),
      Var(70074) ~ (Var(121790) & Var(70072)),
      Var(70076) ~ (Var(121788) & Var(70074)),
      Var(70078) ~ (Var(121786) & Var(70076)),
      Var(70088) ~ Var(121794),
      Var(70091) ~ Var(121796),
      Var(70094) ~ Var(121798),
      Var(70097) ~ Var(121800),
      Var(70100) ~ Var(121802),
      Var(70102) ~ Var(121812),
      Var(70104) ~ (Var(121810) & Var(70102)),
      Var(70106) ~ (Var(121808) & Var(70104)),
      Var(70108) ~ (Var(121806) & Var(70106)),
      Var(70110) ~ (Var(121804) & Var(70108)),
      Var(70118) ~ True,
      Var(70123) ~ True,
      Var(70126) ~ (Var(70118) & Var(70123)),
      Var(70132) ~ True,
      Var(70135) ~ Var(70132)
    )
    val s = solveAll(l).get
    // verify(s, l) -- TOO SLOW
  }


  test("Array.copyOfRange") {
    val l = List(
      (Cst(22553) & Cst(22551)) ~ (Var(90584) & (Var(90592) & (Var(90600) & (Var(90608) & (Var(90616) & (Var(90625) & (Var(90633) & (Var(90641) & (Var(90649) & (Var(90657) & (Var(90666) & (Var(90675) & (Var(90684) & Var(90688)))))))))))))),
      Var(90580) ~ Var(134536),
      Var(90582) ~ (Var(134534) & Var(90580)),
      Var(90584) ~ (Var(134532) & Var(90582)),
      Var(90588) ~ Var(134542),
      Var(90590) ~ (Var(134540) & Var(90588)),
      Var(90592) ~ (Var(134538) & Var(90590)),
      Var(90596) ~ Var(134548),
      Var(90598) ~ (Var(134546) & Var(90596)),
      Var(90600) ~ (Var(134544) & Var(90598)),
      Var(90604) ~ Var(134554),
      Var(90606) ~ (Var(134552) & Var(90604)),
      Var(90608) ~ (Var(134550) & Var(90606)),
      Var(90612) ~ Var(134560),
      Var(90614) ~ (Var(134558) & Var(90612)),
      Var(90616) ~ (Var(134556) & Var(90614)),
      Var(90621) ~ Var(134566),
      Var(90623) ~ (Var(134564) & Var(90621)),
      Var(90625) ~ (Var(134562) & Var(90623)),
      Var(90629) ~ Var(134572),
      Var(90631) ~ (Var(134570) & Var(90629)),
      Var(90633) ~ (Var(134568) & Var(90631)),
      Var(90637) ~ Var(134578),
      Var(90639) ~ (Var(134576) & Var(90637)),
      Var(90641) ~ (Var(134574) & Var(90639)),
      Var(90645) ~ Var(134584),
      Var(90647) ~ (Var(134582) & Var(90645)),
      Var(90649) ~ (Var(134580) & Var(90647)),
      Var(90653) ~ Var(134590),
      Var(90655) ~ (Var(134588) & Var(90653)),
      Var(90657) ~ (Var(134586) & Var(90655)),
      Var(90662) ~ Var(134596),
      Var(90664) ~ (Var(134594) & Var(90662)),
      Var(90666) ~ (Var(134592) & Var(90664)),
      Var(90671) ~ Var(134602),
      Var(90673) ~ (Var(134600) & Var(90671)),
      Var(90675) ~ (Var(134598) & Var(90673)),
      Var(90680) ~ Var(134608),
      Var(90682) ~ (Var(134606) & Var(90680)),
      Var(90684) ~ (Var(134604) & Var(90682)),
      Var(90688) ~ True
    )
    val s = solveAll(l).get
    // verify(s, l) -- TOO SLOW
  }

  test("Fixpoint.Ast.Datalog.toString$299997") {
    val l = List(
      True ~ (Var(100987) & (Var(101022) & Var(101116))),
      Var(100987) ~ (Var(100990) & (Var(100997) & (Var(101007) & Var(101019)))),
      Var(100990) ~ Var(108420),
      Var(100994) ~ True,
      Var(100996) ~ (Var(108422) & Var(108423)),
      Var(100997) ~ Var(101006),
      Var(101001) ~ Var(108430),
      Var(101004) ~ Var(108432),
      Var(101006) ~ (((Var(108427) & Var(108426)) & Var(108428)) & Var(101004)),
      Var(101007) ~ Var(101016),
      Var(101011) ~ Var(108439),
      Var(101014) ~ Var(108441),
      Var(101016) ~ (((Var(108436) & Var(108435)) & Var(108437)) & Var(101014)),
      Var(101019) ~ Var(108443),
      Var(101022) ~ (Var(101025) & (Var(101109) & Var(101112))),
      Var(101025) ~ Var(108444),
      Var(101029) ~ True,
      Var(101036) ~ Var(108463),
      Var(101039) ~ True,
      Var(101041) ~ Var(108461),
      Var(101043) ~ (Var(108458) & Var(101041)),
      Var(101046) ~ True,
      Var(101048) ~ Var(108467),
      Var(101050) ~ (Var(108455) & (Var(101043) & Var(101048))),
      Var(101054) ~ True,
      Var(101059) ~ True,
      Var(101063) ~ (Var(108469) & (Var(101054) & Var(101059))),
      Var(101065) ~ Var(108453),
      Var(101073) ~ Var(108484),
      Var(101076) ~ True,
      Var(101078) ~ Var(108482),
      Var(101080) ~ (Var(108479) & Var(101078)),
      Var(101083) ~ True,
      Var(101085) ~ Var(108488),
      Var(101087) ~ (Var(108476) & (Var(101080) & Var(101085))),
      Var(101091) ~ True,
      Var(101096) ~ True,
      Var(101101) ~ True,
      Var(101105) ~ (Var(108490) & ((Var(101091) & Var(101096)) & Var(101101))),
      Var(101107) ~ Var(108474),
      Var(101109) ~ Var(108447),
      Var(101112) ~ Var(108494),
      Var(101116) ~ (Var(101119) & (Var(101125) & (Var(101131) & Var(101134)))),
      Var(101119) ~ Var(108495),
      Var(101123) ~ True,
      Var(101125) ~ (Var(108496) & Var(101123)),
      Var(101129) ~ True,
      Var(101131) ~ (Var(108498) & Var(101129)),
      Var(101134) ~ Var(108500)
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Nec.zipWithA") {
    val l = List(
      Cst(24685) ~ Var(68027),
      Var(67938) ~ True,
      Var(67940) ~ (Var(120514) & Var(67938)),
      Var(67943) ~ True,
      Var(67945) ~ (Var(120512) & (Var(67940) & Var(67943))),
      Var(67950) ~ True,
      Var(67952) ~ Var(120531),
      Var(67954) ~ (Var(120529) & Var(67952)),
      Var(67956) ~ (Var(120524) & Var(67954)),
      Var(67958) ~ (Var(120521) & Var(67956)),
      Var(67960) ~ (Var(120518) & Var(67958)),
      Var(67962) ~ (Var(120510) & Var(67945)),
      Var(67971) ~ True,
      Var(67973) ~ Var(120542),
      Var(67975) ~ (Var(120540) & Var(67973)),
      Var(67977) ~ (Var(120535) & Var(67975)),
      Var(67979) ~ (Var(120533) & Var(67977)),
      Var(67988) ~ True,
      Var(67990) ~ Var(120553),
      Var(67992) ~ (Var(120551) & Var(67990)),
      Var(67994) ~ (Var(120546) & Var(67992)),
      Var(67996) ~ (Var(120544) & Var(67994)),
      Var(68004) ~ True,
      Var(68006) ~ Var(120564),
      Var(68008) ~ (Var(120562) & Var(68006)),
      Var(68010) ~ (Var(120557) & Var(68008)),
      Var(68012) ~ (Var(120555) & Var(68010)),
      Var(68015) ~ True,
      Var(68017) ~ (Var(120570) & Var(68015)),
      Var(68020) ~ True,
      Var(68022) ~ (Var(120568) & (Var(68017) & Var(68020))),
      Var(68025) ~ Var(120574),
      Var(68027) ~ (Var(120566) & Var(68022))
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Concurrent.Channel.selectHelper") {
    val l = List(
      Cst(1794221043) ~ (Var(85999) & (Var(86002) & (Var(86045) & (Var(86052) & ((Var(86063) & (Var(86069) & (Var(86072) & (Var(86075) & Var(86078))))) & Var(86094)))))),
      Var(85997) ~ Cst(1794221043),
      Var(85999) ~ Var(131629),
      Var(86002) ~ Cst(1794221043),
      Var(86008) ~ Var(131642),
      Var(86020) ~ Var(131646),
      Var(86022) ~ Var(86020),
      Var(86024) ~ Var(131640),
      Var(86026) ~ (Var(131637) & Var(86024)),
      Var(86029) ~ Var(131650),
      Var(86033) ~ Var(131648),
      Var(86035) ~ (Var(131634) & (Var(86026) & Var(86033))),
      Var(86038) ~ Var(131654),
      Var(86041) ~ True,
      Var(86043) ~ Var(131652),
      Var(86045) ~ (Var(131631) & (Var(86035) & Var(86043))),
      Var(86052) ~ True,
      Var(86057) ~ Cst(1794221043),
      Var(86059) ~ Var(131668),
      Var(86061) ~ (Var(131666) & Var(86059)),
      Var(86063) ~ (Var(131664) & Var(86061)),
      Var(86067) ~ Cst(1794221043),
      Var(86069) ~ Var(131670),
      Var(86072) ~ Cst(1794221043),
      Var(86075) ~ Cst(1794221043),
      Var(86078) ~ Cst(1794221043),
      Var(86084) ~ True,
      Var(86087) ~ Var(131680),
      Var(86090) ~ Cst(1794221043),
      Var(86092) ~ Var(131678),
      Var(86094) ~ (Var(131672) & (Var(86084) & Var(86092)))
    )
    val s = solveAll(l).get
    verify(s, l)
  }


  test("Array.transpose") {
    val l = List(
      ((Cst(21536) & Cst(21537)) & Cst(21534)) ~ (Var(65522) & (Var(65525) & (Var(65527) & (Var(65533) & ((Var(65536) & Var(65539)) & (Var(65555) & Var(65577))))))),
      Var(65522) ~ True,
      Var(65525) ~ True,
      Var(65527) ~ Var(119096),
      Var(65531) ~ Var(119101),
      Var(65533) ~ Var(65531),
      Var(65536) ~ True,
      Var(65539) ~ Var(119105),
      Var(65546) ~ Var(119115),
      Var(65548) ~ Var(65546),
      Var(65551) ~ Var(119117),
      Var(65553) ~ ((Var(119111) & Var(119109)) & (Var(65548) & Var(65551))),
      Var(65555) ~ (Var(119107) & Var(119106)),
      Var(65562) ~ Var(119133),
      Var(65564) ~ Var(119131),
      Var(65566) ~ (Var(119128) & Var(65564)),
      Var(65569) ~ Var(119137),
      Var(65571) ~ Var(119135),
      Var(65573) ~ (Var(119125) & (Var(65566) & Var(65571))),
      Var(65575) ~ (Var(119122) & Var(119121)),
      Var(65577) ~ (Var(119119) & Var(119118))
    )
    val s = solveAll(l).get
    verify(s, l)
  }


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
    // verify(s, l) -- TOO SLOW
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
