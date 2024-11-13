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

  import Term.*

  private implicit val defaultLoc: SourceLocation = SourceLocation.Unknown

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
    val s = solveAll(l).unsafeGet
    // verify(s, l) -- TOO SLOW
  }

  test("Array.patch") {
    val l = List(
      ((Cst(21405) & Cst(21406)) & Cst(21403)) ~ (Var(87075) & (Var(87078) & (Var(87081) & Var(87084)))),
      Var(87075) ~ True,
      Var(87078) ~ True,
      Var(87081) ~ (Var(132325) & Var(132323)),
      Var(87084) ~ ((Var(132328) & Var(132329)) & Var(132326))
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Boxable.lift1") {
    val l = List(
      Var(56474) ~ True,
      Var(56476) ~ (Var(113308) & Var(56474)),
      Var(56478) ~ Var(56476)
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Concurrent.Channel.newChannel") {
    val l = List(
      Cst(1794221043) ~ ((Var(68958) & Var(68961)) & (Var(68964) & (Var(68970) & (Var(68972) & ((Var(68974) & (Var(68977) & (Var(68980) & Var(68983)))) & Var(68987)))))),
      Var(68958) ~ True,
      Var(68961) ~ True,
      Var(68964) ~ True,
      Var(68968) ~ True,
      Var(68970) ~ (Cst(1794221043) & Var(68968)),
      Var(68972) ~ Var(121159),
      Var(68974) ~ Var(121162),
      Var(68977) ~ Var(121163),
      Var(68980) ~ Cst(1794221043),
      Var(68983) ~ Cst(1794221043),
      Var(68987) ~ Var(121165)
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Debug.escape") {
    val l = List(
      True ~ Var(90246),
      Var(90195) ~ Var(134303),
      Var(90197) ~ True,
      Var(90199) ~ (Var(134301) & (Var(90195) & Var(90197))),
      Var(90201) ~ True,
      Var(90203) ~ (Var(134299) & (Var(90199) & Var(90201))),
      Var(134304) ~ Var(90203),
      Var(90212) ~ Var(134326),
      Var(90214) ~ (Var(134324) & Var(90212)),
      Var(90216) ~ (Var(134321) & Var(90214)),
      Var(90218) ~ Var(134330),
      Var(90220) ~ (Var(134328) & Var(90218)),
      Var(90222) ~ (Var(134318) & (Var(90216) & Var(90220))),
      Var(90224) ~ Var(134334),
      Var(90226) ~ (Var(134332) & Var(90224)),
      Var(90228) ~ (Var(134315) & (Var(90222) & Var(90226))),
      Var(90230) ~ Var(134338),
      Var(90232) ~ (Var(134336) & Var(90230)),
      Var(90234) ~ (Var(134312) & (Var(90228) & Var(90232))),
      Var(90236) ~ Var(134342),
      Var(90238) ~ (Var(134340) & Var(90236)),
      Var(90240) ~ (Var(134309) & (Var(90234) & Var(90238))),
      Var(90242) ~ Var(134346),
      Var(90244) ~ (Var(134344) & Var(90242)),
      Var(90246) ~ (Var(134306) & (Var(90240) & Var(90244)))
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Files.readChunks") {
    val l = List(
      (Cst(1794221043) & Cst(8354)) ~ (Var(50222) & (((Var(50227) & (Var(50229) & (Var(50234) & (Var(50242) & Var(50280))))) & Var(50292)) & Var(50301))),
      Var(50222) ~ True,
      Var(50285) ~ Var(109424),
      Var(50290) ~ Var(50285),
      Var(50292) ~ Var(50290),
      Var(50227) ~ Var(109426),
      Var(50229) ~ Var(109428),
      Var(50232) ~ Var(109430),
      Var(50234) ~ (Var(50232) & Var(109429)),
      Var(50236) ~ Var(109436),
      Var(50238) ~ Var(109438),
      Var(50240) ~ (Var(109434) & (Var(50236) & Var(50238))),
      Var(50242) ~ (Var(50240) & Var(109432)),
      Var(50245) ~ Var(109441),
      Var(50247) ~ Var(50245),
      Var(50268) ~ Var(109443),
      Var(50271) ~ Var(109448),
      Var(50249) ~ Var(109450),
      Var(50252) ~ Var(109453),
      Var(50253) ~ (Var(50252) & Var(109452)),
      Var(50255) ~ Var(109456),
      Var(50257) ~ Var(109462),
      Var(50259) ~ Var(109464),
      Var(50261) ~ (Var(109460) & (Var(50257) & Var(50259))),
      Var(50262) ~ (Var(50261) & Var(109458)),
      Var(50265) ~ Var(109468),
      Var(50280) ~ True,
      Var(50299) ~ True,
      Var(50301) ~ Var(50299)
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Fixpoint.Interpreter.evalStmt") {
    val l = List(
      Cst(164) ~ (Var(73972) & ((Var(73981) & (Var(73984) & (Var(73994) & Var(74008)))) & (Var(74020) & (Var(74025) & (Var(74037) & (Var(74047) & (Var(74050) & Var(74053)))))))),
      Var(73970) ~ Var(124149),
      Var(73972) ~ ((Var(124146) & Var(124148)) & Var(73970)),
      Var(73979) ~ Var(124154),
      Var(73981) ~ (Var(124153) & Var(73979)),
      Var(73984) ~ True,
      Var(73990) ~ Var(124167),
      Var(73992) ~ (Var(124166) & Var(73990)),
      Var(73994) ~ ((Var(124162) & Var(124163)) & Var(73992)),
      Var(74004) ~ Var(124178),
      Var(74006) ~ (Var(124177) & Var(74004)),
      Var(74008) ~ (((Var(124171) & Var(124173)) & Var(124174)) & Var(74006)),
      Var(74016) ~ Var(124187),
      Var(74018) ~ (Var(124186) & Var(74016)),
      Var(74020) ~ (Var(124183) & Var(74018)),
      Var(74025) ~ Var(124192),
      Var(74031) ~ Var(124199),
      Var(74033) ~ Var(124198),
      Var(74035) ~ (Var(124196) & Var(74033)),
      Var(74037) ~ (Var(124194) & Var(74035)),
      Var(74043) ~ Var(124204),
      Var(74045) ~ Var(124206),
      Var(74047) ~ ((Var(124201) & Var(124203)) & (Var(74043) & Var(74045))),
      Var(74050) ~ Var(124208),
      Var(74053) ~ Var(124210)
    )
    val s = solveAll(l).unsafeGet
    // verify(s, l) -- TOO SLOW
  }

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
    val s = solveAll(l).unsafeGet
    // verify(s, l) -- TOO SLOW
  }

  test("Fixpoint.Phase.Compiler.compileRuleIncr") {
    val l = List(
      Cst(14919) ~ (Var(69065) & (Var(69068) & (Var(69076) & (Var(69079) & (Var(69084) & (Var(69091) & (Var(69150) & (Var(69153) & ((Var(69156) & (Var(69160) & Var(69167))) & Var(69174)))))))))),
      Var(69065) ~ True,
      Var(69068) ~ True,
      Var(69072) ~ True,
      Var(69074) ~ Var(121219),
      Var(69076) ~ (Var(121216) & Var(69074)),
      Var(69079) ~ True,
      Var(69084) ~ True,
      Var(69089) ~ True,
      Var(69091) ~ Var(69089),
      Var(69105) ~ True,
      Var(69108) ~ True,
      Var(69111) ~ True,
      Var(69124) ~ True,
      Var(69126) ~ Var(69124),
      Var(69128) ~ Var(121244),
      Var(69132) ~ Var(121234),
      Var(69135) ~ (Var(121230) & Var(69132)),
      Var(69138) ~ Var(121252),
      Var(69150) ~ Var(121254),
      Var(69153) ~ True,
      Var(69156) ~ True,
      Var(69160) ~ Var(121259),
      Var(69165) ~ Var(121263),
      Var(69167) ~ Var(121261),
      Var(69172) ~ Var(121267),
      Var(69174) ~ Var(121265)
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Fixpoint.Phase.IndexSelection.queryOp") {
    val l = List(
      True ~ ((Var(66558) & (Var(66568) & (Var(66628) & (Var(66633) & (Var(66638) & (Var(66641) & (Var(66644) & (Var(66657) & Var(66660))))))))) & (Var(66668) & (Var(66673) & (Var(66678) & (Var(66684) & Var(66689)))))),
      Var(66556) ~ True,
      Var(66558) ~ Var(66556),
      Var(66564) ~ True,
      Var(66566) ~ Var(119740),
      Var(66568) ~ (Var(119738) & Var(66566)),
      Var(66582) ~ True,
      Var(66597) ~ Var(119746),
      Var(66600) ~ Var(119764),
      Var(66610) ~ True,
      Var(66613) ~ True,
      Var(66623) ~ True,
      Var(66626) ~ Var(119762),
      Var(66628) ~ (Var(119743) & (Var(66597) & Var(66626))),
      Var(66633) ~ True,
      Var(66638) ~ True,
      Var(66641) ~ True,
      Var(66644) ~ True,
      Var(66657) ~ Var(119782),
      Var(66660) ~ True,
      Var(66668) ~ True,
      Var(66673) ~ True,
      Var(66678) ~ True,
      Var(66684) ~ True,
      Var(66689) ~ True
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Fixpoint.Phase.Simplifier.simplifyOp") {
    val l = List(
      True ~ ((Var(58593) & (Var(58598) & Var(58601))) & (Var(58635) & (Var(58667) & Var(58683)))),
      Var(58568) ~ True,
      Var(58576) ~ True,
      Var(58579) ~ True,
      Var(58583) ~ Var(114603),
      Var(58586) ~ Var(114615),
      Var(58591) ~ Var(114613),
      Var(58593) ~ (Var(114600) & (Var(58583) & Var(58591))),
      Var(58598) ~ True,
      Var(58601) ~ True,
      Var(58613) ~ True,
      Var(58626) ~ True,
      Var(58628) ~ Var(114640),
      Var(58630) ~ (Var(114635) & Var(58628)),
      Var(58633) ~ True,
      Var(58635) ~ (Var(114631) & Var(58633)),
      Var(58645) ~ True,
      Var(58658) ~ True,
      Var(58660) ~ Var(114660),
      Var(58662) ~ (Var(114655) & Var(58660)),
      Var(58665) ~ True,
      Var(58667) ~ (Var(114651) & Var(58665)),
      Var(58678) ~ True,
      Var(58681) ~ True,
      Var(58683) ~ (Var(114664) & Var(58681))
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Fixpoint.Solver.solveWithStratification") {
    val l = List(
      True ~ (Var(65701) & (Var(65724) & Var(65762))),
      Var(65677) ~ Cst(1794221043),
      Var(65683) ~ True,
      Var(65686) ~ True,
      Var(65688) ~ (Var(119207) & Var(65683)),
      Var(65691) ~ True,
      Var(65693) ~ (Var(119204) & Var(65688)),
      Var(65696) ~ True,
      Var(65698) ~ (Var(119201) & Var(65693)),
      Var(65701) ~ Var(65717),
      Var(65705) ~ Var(119224),
      Var(65708) ~ Var(119227),
      Var(65710) ~ Var(119226),
      Var(65712) ~ (Var(119221) & (Var(65705) & Var(65710))),
      Var(65715) ~ Var(119230),
      Var(65717) ~ (Var(119218) & Var(65712)),
      Var(65724) ~ (Var(65740) & Var(65758)),
      Var(65729) ~ Var(119240),
      Var(65731) ~ Var(119239),
      Var(65733) ~ (Var(119235) & Var(65731)),
      Var(65736) ~ Var(119245),
      Var(65738) ~ Var(119244),
      Var(65740) ~ (Var(119232) & (Var(65733) & Var(65738))),
      Var(65744) ~ Var(119255),
      Var(65747) ~ Var(119260),
      Var(65749) ~ Var(119259),
      Var(65751) ~ (Var(119257) & Var(65749)),
      Var(65753) ~ (Var(119252) & (Var(65744) & Var(65751))),
      Var(65756) ~ Var(119263),
      Var(65758) ~ (Var(119249) & Var(65753)),
      Var(65762) ~ True,
      Var(65765) ~ Cst(1794221043)
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Iterable.enumerator") {
    val l = List(
      Var(101438) ~ Var(101439),
      Var(101434) ~ Var(101437),
      Var(101431) ~ Cst(101435),
      Var(101438) ~ Cst(101435),
      Var(101433) ~ Var(101437),
      Var(101434) ~ Var(101438),
      Var(101434) ~ Cst(101435),
      Var(101432) ~ (Cst(101436) & Var(101438)),
      Var(101431) ~ (Var(101433) & Var(101439)),
      (Cst(101436) & Cst(101435)) ~ (Var(101434) & Var(101432))
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("Iterator.next") {
    val l = List(
      (Cst(1435) & Cst(1436)) ~ Var(55261),
      Var(55251) ~ Var(112576),
      Var(55257) ~ Var(112582),
      Var(55261) ~ Var(112585)
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("List.merge") {
    val l = List(
      True ~ Var(86467),
      Var(86412) ~ True,
      Var(86416) ~ True,
      Var(86420) ~ True,
      Var(86423) ~ Var(131895),
      Var(86425) ~ (Var(131893) & Var(86423)),
      Var(86428) ~ Var(131897),
      Var(86430) ~ (Var(131891) & Var(86425)),
      Var(86432) ~ Var(131904),
      Var(86434) ~ (Var(131902) & Var(86432)),
      Var(86437) ~ Var(131906),
      Var(86439) ~ (Var(131900) & Var(86434)),
      Var(86446) ~ Var(131909),
      Var(86453) ~ Var(131912),
      Var(86458) ~ Var(131915),
      Var(86460) ~ Var(131922),
      Var(86462) ~ (Var(131920) & Var(86460)),
      Var(86465) ~ True,
      Var(86467) ~ (Var(131918) & Var(86462))
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("List.scanRight") {
    val l = List(
      Var(144695) ~ True,
      Var(144681) ~ Var(144691),
      Var(144684) ~ True,
      Var(144677) ~ Var(144690),
      Var(144680) ~ Var(144696),
      Var(144684) ~ Var(144693),
      Var(144679) ~ Var(144695),
      Var(144682) ~ Var(144691),
      Var(144688) ~ True,
      Var(144685) ~ Cst(144689),
      Var(144683) ~ Cst(144689),
      Var(144680) ~ Var(144695),
      Var(144677) ~ Var(144691),
      True ~ True,
      True ~ True,
      True ~ True,
      True ~ True,
      True ~ True,
      True ~ True,
      Var(144691) ~ (Var(144678) & Var(144688)),
      Var(144692) ~ (Var(144682) & Var(144687)),
      Var(144694) ~ (Var(144682) & Var(144687)),
      Var(144685) ~ (Var(144696) & Var(144694)),
      Var(144687) ~ (Var(144679) & Var(144692)),
      Var(144678) ~ (Var(144681) & Var(144686)),
      Var(144686) ~ (Var(144683) & Var(144693))
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    // verify(s, l) -- TOO SLOW
  }

  test("MutDeque.toArray") {
    val l = List(
      (Cst(915) & Cst(913)) ~ (Var(85240) & (Var(85242) & (Var(85244) & (Var(85247) & (Var(85249) & (Var(85252) & (Var(85257) & Var(85270)))))))),
      Var(85240) ~ Var(131193),
      Var(85242) ~ Var(131195),
      Var(85244) ~ Var(131197),
      Var(85247) ~ True,
      Var(85249) ~ Var(131199),
      Var(85252) ~ True,
      Var(85255) ~ Var(131206),
      Var(85257) ~ ((Var(131204) & Var(131202)) & Var(85255)),
      Var(85261) ~ Var(131215),
      Var(85263) ~ ((Var(131213) & Var(131211)) & Var(85261)),
      Var(85266) ~ Var(131220),
      Var(85268) ~ ((Var(131218) & Var(131216)) & Var(85266)),
      Var(85270) ~ (((Var(131209) & Var(131210)) & Var(131207)) & (Var(85263) & Var(85268)))
    )
    val s = solveAll(l).unsafeGet
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
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

  test("RedBlackTree.insertWith") {
    val l = List(
      Cst(39139) ~ Var(73552),
      Var(73498) ~ Var(123880),
      Var(73507) ~ True,
      Var(73510) ~ Var(123891),
      Var(73514) ~ True,
      Var(73516) ~ (Var(123893) & Var(73514)),
      Var(73518) ~ (Var(123889) & Var(73510)),
      Var(73521) ~ Var(123907),
      Var(73523) ~ (Var(123905) & Var(73521)),
      Var(73525) ~ (Var(123903) & Var(73523)),
      Var(73528) ~ (Var(123899) & Var(73525)),
      Var(73531) ~ Var(123911),
      Var(73535) ~ True,
      Var(73537) ~ (Var(123913) & Var(73535)),
      Var(73539) ~ (Var(123909) & Var(73531)),
      Var(73542) ~ Var(123919),
      Var(73545) ~ Var(123925),
      Var(73548) ~ True,
      Var(73550) ~ (Var(123923) & Var(73545)),
      Var(73552) ~ Var(73550)
    )
    val s = solveAll(l).unsafeGet
    verify(s, l)
  }

}
