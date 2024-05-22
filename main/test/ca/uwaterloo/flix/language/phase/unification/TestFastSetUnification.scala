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
import ca.uwaterloo.flix.language.phase.unification.FastSetUnification.{Term, solveAll, verify}
import org.scalatest.funsuite.AnyFunSuite

class TestFastSetUnification extends AnyFunSuite with TestUtils {

  import Term._

  private implicit val defaultLoc: SourceLocation = SourceLocation.Unknown

  ignore("Array.copyOfRange") {
    val l = List(
      (Cst(22553) union Cst(22551)) ~ (Var(90584) union (Var(90592) union (Var(90600) union (Var(90608) union (Var(90616) union (Var(90625) union (Var(90633) union (Var(90641) union (Var(90649) union (Var(90657) union (Var(90666) union (Var(90675) union (Var(90684) union Var(90688)))))))))))))),
      Var(90580) ~ Var(134536),
      Var(90582) ~ (Var(134534) union Var(90580)),
      Var(90584) ~ (Var(134532) union Var(90582)),
      Var(90588) ~ Var(134542),
      Var(90590) ~ (Var(134540) union Var(90588)),
      Var(90592) ~ (Var(134538) union Var(90590)),
      Var(90596) ~ Var(134548),
      Var(90598) ~ (Var(134546) union Var(90596)),
      Var(90600) ~ (Var(134544) union Var(90598)),
      Var(90604) ~ Var(134554),
      Var(90606) ~ (Var(134552) union Var(90604)),
      Var(90608) ~ (Var(134550) union Var(90606)),
      Var(90612) ~ Var(134560),
      Var(90614) ~ (Var(134558) union Var(90612)),
      Var(90616) ~ (Var(134556) union Var(90614)),
      Var(90621) ~ Var(134566),
      Var(90623) ~ (Var(134564) union Var(90621)),
      Var(90625) ~ (Var(134562) union Var(90623)),
      Var(90629) ~ Var(134572),
      Var(90631) ~ (Var(134570) union Var(90629)),
      Var(90633) ~ (Var(134568) union Var(90631)),
      Var(90637) ~ Var(134578),
      Var(90639) ~ (Var(134576) union Var(90637)),
      Var(90641) ~ (Var(134574) union Var(90639)),
      Var(90645) ~ Var(134584),
      Var(90647) ~ (Var(134582) union Var(90645)),
      Var(90649) ~ (Var(134580) union Var(90647)),
      Var(90653) ~ Var(134590),
      Var(90655) ~ (Var(134588) union Var(90653)),
      Var(90657) ~ (Var(134586) union Var(90655)),
      Var(90662) ~ Var(134596),
      Var(90664) ~ (Var(134594) union Var(90662)),
      Var(90666) ~ (Var(134592) union Var(90664)),
      Var(90671) ~ Var(134602),
      Var(90673) ~ (Var(134600) union Var(90671)),
      Var(90675) ~ (Var(134598) union Var(90673)),
      Var(90680) ~ Var(134608),
      Var(90682) ~ (Var(134606) union Var(90680)),
      Var(90684) ~ (Var(134604) union Var(90682)),
      Var(90688) ~ Empty
    )
    val s = solveAll(l).get
    // verify(s, l) -- TOO SLOW
  }

  test("Array.patch") {
    val l = List(
      ((Cst(21405) union Cst(21406)) union Cst(21403)) ~ (Var(87075) union (Var(87078) union (Var(87081) union Var(87084)))),
      Var(87075) ~ Empty,
      Var(87078) ~ Empty,
      Var(87081) ~ (Var(132325) union Var(132323)),
      Var(87084) ~ ((Var(132328) union Var(132329)) union Var(132326))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Array.transpose") {
    val l = List(
      ((Cst(21536) union Cst(21537)) union Cst(21534)) ~ (Var(65522) union (Var(65525) union (Var(65527) union (Var(65533) union ((Var(65536) union Var(65539)) union (Var(65555) union Var(65577))))))),
      Var(65522) ~ Empty,
      Var(65525) ~ Empty,
      Var(65527) ~ Var(119096),
      Var(65531) ~ Var(119101),
      Var(65533) ~ Var(65531),
      Var(65536) ~ Empty,
      Var(65539) ~ Var(119105),
      Var(65546) ~ Var(119115),
      Var(65548) ~ Var(65546),
      Var(65551) ~ Var(119117),
      Var(65553) ~ ((Var(119111) union Var(119109)) union (Var(65548) union Var(65551))),
      Var(65555) ~ (Var(119107) union Var(119106)),
      Var(65562) ~ Var(119133),
      Var(65564) ~ Var(119131),
      Var(65566) ~ (Var(119128) union Var(65564)),
      Var(65569) ~ Var(119137),
      Var(65571) ~ Var(119135),
      Var(65573) ~ (Var(119125) union (Var(65566) union Var(65571))),
      Var(65575) ~ (Var(119122) union Var(119121)),
      Var(65577) ~ (Var(119119) union Var(119118))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Boxable.lift1") {
    val l = List(
      Var(56474) ~ Empty,
      Var(56476) ~ (Var(113308) union Var(56474)),
      Var(56478) ~ Var(56476)
    )
    val s = solveAll(l).get
//    println(s)
    verify(s, l)
  }

  test("Concurrent.Channel.newChannel") {
    val l = List(
      Cst(1794221043) ~ ((Var(68958) union Var(68961)) union (Var(68964) union (Var(68970) union (Var(68972) union ((Var(68974) union (Var(68977) union (Var(68980) union Var(68983)))) union Var(68987)))))),
      Var(68958) ~ Empty,
      Var(68961) ~ Empty,
      Var(68964) ~ Empty,
      Var(68968) ~ Empty,
      Var(68970) ~ (Cst(1794221043) union Var(68968)),
      Var(68972) ~ Var(121159),
      Var(68974) ~ Var(121162),
      Var(68977) ~ Var(121163),
      Var(68980) ~ Cst(1794221043),
      Var(68983) ~ Cst(1794221043),
      Var(68987) ~ Var(121165)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Concurrent.Channel.selectHelper") {
    val l = List(
      Cst(1794221043) ~ (Var(85999) union (Var(86002) union (Var(86045) union (Var(86052) union ((Var(86063) union (Var(86069) union (Var(86072) union (Var(86075) union Var(86078))))) union Var(86094)))))),
      Var(85997) ~ Cst(1794221043),
      Var(85999) ~ Var(131629),
      Var(86002) ~ Cst(1794221043),
      Var(86008) ~ Var(131642),
      Var(86020) ~ Var(131646),
      Var(86022) ~ Var(86020),
      Var(86024) ~ Var(131640),
      Var(86026) ~ (Var(131637) union Var(86024)),
      Var(86029) ~ Var(131650),
      Var(86033) ~ Var(131648),
      Var(86035) ~ (Var(131634) union (Var(86026) union Var(86033))),
      Var(86038) ~ Var(131654),
      Var(86041) ~ Empty,
      Var(86043) ~ Var(131652),
      Var(86045) ~ (Var(131631) union (Var(86035) union Var(86043))),
      Var(86052) ~ Empty,
      Var(86057) ~ Cst(1794221043),
      Var(86059) ~ Var(131668),
      Var(86061) ~ (Var(131666) union Var(86059)),
      Var(86063) ~ (Var(131664) union Var(86061)),
      Var(86067) ~ Cst(1794221043),
      Var(86069) ~ Var(131670),
      Var(86072) ~ Cst(1794221043),
      Var(86075) ~ Cst(1794221043),
      Var(86078) ~ Cst(1794221043),
      Var(86084) ~ Empty,
      Var(86087) ~ Var(131680),
      Var(86090) ~ Cst(1794221043),
      Var(86092) ~ Var(131678),
      Var(86094) ~ (Var(131672) union (Var(86084) union Var(86092)))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Debug.escape") {
    val l = List(
      Empty ~ Var(90246),
      Var(90195) ~ Var(134303),
      Var(90197) ~ Empty,
      Var(90199) ~ (Var(134301) union (Var(90195) union Var(90197))),
      Var(90201) ~ Empty,
      Var(90203) ~ (Var(134299) union (Var(90199) union Var(90201))),
      Var(134304) ~ Var(90203),
      Var(90212) ~ Var(134326),
      Var(90214) ~ (Var(134324) union Var(90212)),
      Var(90216) ~ (Var(134321) union Var(90214)),
      Var(90218) ~ Var(134330),
      Var(90220) ~ (Var(134328) union Var(90218)),
      Var(90222) ~ (Var(134318) union (Var(90216) union Var(90220))),
      Var(90224) ~ Var(134334),
      Var(90226) ~ (Var(134332) union Var(90224)),
      Var(90228) ~ (Var(134315) union (Var(90222) union Var(90226))),
      Var(90230) ~ Var(134338),
      Var(90232) ~ (Var(134336) union Var(90230)),
      Var(90234) ~ (Var(134312) union (Var(90228) union Var(90232))),
      Var(90236) ~ Var(134342),
      Var(90238) ~ (Var(134340) union Var(90236)),
      Var(90240) ~ (Var(134309) union (Var(90234) union Var(90238))),
      Var(90242) ~ Var(134346),
      Var(90244) ~ (Var(134344) union Var(90242)),
      Var(90246) ~ (Var(134306) union (Var(90240) union Var(90244)))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Files.append") {
    val l = List(
      Cst(1794221043) ~ ((Var(55040) union (Var(55042) union (Var(55046) union (Var(55050) union (Var(55058) union (Var(55060) union (Var(55062) union (Var(55066) union Var(55075))))))))) union Var(55078)),
      Var(55078) ~ Var(112431),
      Var(55040) ~ Cst(1794221043),
      Var(55042) ~ Var(112433),
      Var(55044) ~ Var(112437),
      Var(55046) ~ (Var(112435) union Var(55044)),
      Var(55048) ~ Empty,
      Var(55050) ~ (Var(112439) union Var(55048)),
      Var(55052) ~ Var(112443),
      Var(55055) ~ Empty,
      Var(55058) ~ (Var(112441) union (Var(55052) union Var(55055))),
      Var(55060) ~ Var(112446),
      Var(55062) ~ Var(112448),
      Var(55066) ~ Empty,
      Var(55075) ~ Var(112453)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Files.readChunks") {
    val l = List(
      (Cst(1794221043) union Cst(8354)) ~ (Var(50222) union (((Var(50227) union (Var(50229) union (Var(50234) union (Var(50242) union Var(50280))))) union Var(50292)) union Var(50301))),
      Var(50222) ~ Empty,
      Var(50285) ~ Var(109424),
      Var(50290) ~ Var(50285),
      Var(50292) ~ Var(50290),
      Var(50227) ~ Var(109426),
      Var(50229) ~ Var(109428),
      Var(50232) ~ Var(109430),
      Var(50234) ~ (Var(50232) union Var(109429)),
      Var(50236) ~ Var(109436),
      Var(50238) ~ Var(109438),
      Var(50240) ~ (Var(109434) union (Var(50236) union Var(50238))),
      Var(50242) ~ (Var(50240) union Var(109432)),
      Var(50245) ~ Var(109441),
      Var(50247) ~ Var(50245),
      Var(50268) ~ Var(109443),
      Var(50271) ~ Var(109448),
      Var(50249) ~ Var(109450),
      Var(50252) ~ Var(109453),
      Var(50253) ~ (Var(50252) union Var(109452)),
      Var(50255) ~ Var(109456),
      Var(50257) ~ Var(109462),
      Var(50259) ~ Var(109464),
      Var(50261) ~ (Var(109460) union (Var(50257) union Var(50259))),
      Var(50262) ~ (Var(50261) union Var(109458)),
      Var(50265) ~ Var(109468),
      Var(50280) ~ Empty,
      Var(50299) ~ Empty,
      Var(50301) ~ Var(50299)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Fixpoint.Ast.Datalog.predSymsOf$29898") {
    val l = List(
      Empty ~ (Var(97799) union Var(97816) union Var(97819) union Var(97847) union Var(97859)),
      Var(97787) ~ Var(107197),
      Var(97790) ~ Empty,
      Var(97792) ~ Var(107195),
      Var(97794) ~ (Var(107192) union Var(97792)),
      Var(97797) ~ Empty,
      Var(97799) ~ (Var(107189) union Var(97794)),
      Var(97804) ~ Var(107211),
      Var(97807) ~ Empty,
      Var(97809) ~ Var(107209),
      Var(97811) ~ (Var(107206) union Var(97809)),
      Var(97814) ~ Empty,
      Var(97816) ~ (Var(107203) union Var(97811)),
      Var(97819) ~ Empty,
      Var(97827) ~ Empty,
      Var(97830) ~ Empty,
      Var(97832) ~ (Var(107224) union Var(97827)),
      Var(97835) ~ Var(107232),
      Var(97838) ~ Empty,
      Var(97840) ~ Var(107230),
      Var(97842) ~ (Var(107221) union Var(97832) union Var(97840)),
      Var(97845) ~ Empty,
      Var(97847) ~ (Var(107218) union Var(97842)),
      Var(97854) ~ Empty,
      Var(97857) ~ Empty,
      Var(97859) ~ (Var(97854) union Var(97857))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Fixpoint.Ast.Datalog.toString$299997") {
    val l = List(
      Empty ~ (Var(100987) union (Var(101022) union Var(101116))),
      Var(100987) ~ (Var(100990) union (Var(100997) union (Var(101007) union Var(101019)))),
      Var(100990) ~ Var(108420),
      Var(100994) ~ Empty,
      Var(100996) ~ (Var(108422) union Var(108423)),
      Var(100997) ~ Var(101006),
      Var(101001) ~ Var(108430),
      Var(101004) ~ Var(108432),
      Var(101006) ~ (((Var(108427) union Var(108426)) union Var(108428)) union Var(101004)),
      Var(101007) ~ Var(101016),
      Var(101011) ~ Var(108439),
      Var(101014) ~ Var(108441),
      Var(101016) ~ (((Var(108436) union Var(108435)) union Var(108437)) union Var(101014)),
      Var(101019) ~ Var(108443),
      Var(101022) ~ (Var(101025) union (Var(101109) union Var(101112))),
      Var(101025) ~ Var(108444),
      Var(101029) ~ Empty,
      Var(101036) ~ Var(108463),
      Var(101039) ~ Empty,
      Var(101041) ~ Var(108461),
      Var(101043) ~ (Var(108458) union Var(101041)),
      Var(101046) ~ Empty,
      Var(101048) ~ Var(108467),
      Var(101050) ~ (Var(108455) union (Var(101043) union Var(101048))),
      Var(101054) ~ Empty,
      Var(101059) ~ Empty,
      Var(101063) ~ (Var(108469) union (Var(101054) union Var(101059))),
      Var(101065) ~ Var(108453),
      Var(101073) ~ Var(108484),
      Var(101076) ~ Empty,
      Var(101078) ~ Var(108482),
      Var(101080) ~ (Var(108479) union Var(101078)),
      Var(101083) ~ Empty,
      Var(101085) ~ Var(108488),
      Var(101087) ~ (Var(108476) union (Var(101080) union Var(101085))),
      Var(101091) ~ Empty,
      Var(101096) ~ Empty,
      Var(101101) ~ Empty,
      Var(101105) ~ (Var(108490) union ((Var(101091) union Var(101096)) union Var(101101))),
      Var(101107) ~ Var(108474),
      Var(101109) ~ Var(108447),
      Var(101112) ~ Var(108494),
      Var(101116) ~ (Var(101119) union (Var(101125) union (Var(101131) union Var(101134)))),
      Var(101119) ~ Var(108495),
      Var(101123) ~ Empty,
      Var(101125) ~ (Var(108496) union Var(101123)),
      Var(101129) ~ Empty,
      Var(101131) ~ (Var(108498) union Var(101129)),
      Var(101134) ~ Var(108500)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Fixpoint.Interpreter.evalStmt") {
    val l = List(
      Cst(164) ~ (Var(73972) union ((Var(73981) union (Var(73984) union (Var(73994) union Var(74008)))) union (Var(74020) union (Var(74025) union (Var(74037) union (Var(74047) union (Var(74050) union Var(74053)))))))),
      Var(73970) ~ Var(124149),
      Var(73972) ~ ((Var(124146) union Var(124148)) union Var(73970)),
      Var(73979) ~ Var(124154),
      Var(73981) ~ (Var(124153) union Var(73979)),
      Var(73984) ~ Empty,
      Var(73990) ~ Var(124167),
      Var(73992) ~ (Var(124166) union Var(73990)),
      Var(73994) ~ ((Var(124162) union Var(124163)) union Var(73992)),
      Var(74004) ~ Var(124178),
      Var(74006) ~ (Var(124177) union Var(74004)),
      Var(74008) ~ (((Var(124171) union Var(124173)) union Var(124174)) union Var(74006)),
      Var(74016) ~ Var(124187),
      Var(74018) ~ (Var(124186) union Var(74016)),
      Var(74020) ~ (Var(124183) union Var(74018)),
      Var(74025) ~ Var(124192),
      Var(74031) ~ Var(124199),
      Var(74033) ~ Var(124198),
      Var(74035) ~ (Var(124196) union Var(74033)),
      Var(74037) ~ (Var(124194) union Var(74035)),
      Var(74043) ~ Var(124204),
      Var(74045) ~ Var(124206),
      Var(74047) ~ ((Var(124201) union Var(124203)) union (Var(74043) union Var(74045))),
      Var(74050) ~ Var(124208),
      Var(74053) ~ Var(124210)
    )
    val s = solveAll(l).get
    // verify(s, l) -- TOO SLOW
  }

  test("Fixpoint.Interpreter.evalTerm") {
    val l = List(
      Cst(442) ~ (Var(69984) union (Var(69992) union (Var(70006) union (Var(70010) union ((Var(70016) union Var(70018)) union ((Var(70025) union (Var(70028) union Var(70032))) union ((Var(70040) union (Var(70043) union (Var(70046) union Var(70052)))) union ((Var(70061) union (Var(70064) union (Var(70067) union (Var(70070) union Var(70078))))) union ((Var(70088) union (Var(70091) union (Var(70094) union (Var(70097) union (Var(70100) union Var(70110)))))) union (Var(70126) union Var(70135))))))))))),
      Var(69982) ~ Var(121740),
      Var(69984) ~ Var(69982),
      Var(69992) ~ Var(121742),
      Var(69999) ~ Var(121748),
      Var(70001) ~ (Var(121746) union Var(69999)),
      Var(70004) ~ Var(121750),
      Var(70006) ~ (Var(121744) union (Var(70001) union Var(70004))),
      Var(70010) ~ Var(121752),
      Var(70016) ~ Var(121754),
      Var(70018) ~ Var(121756),
      Var(70025) ~ Var(121758),
      Var(70028) ~ Var(121760),
      Var(70030) ~ Var(121764),
      Var(70032) ~ (Var(121762) union Var(70030)),
      Var(70040) ~ Var(121766),
      Var(70043) ~ Var(121768),
      Var(70046) ~ Var(121770),
      Var(70048) ~ Var(121776),
      Var(70050) ~ (Var(121774) union Var(70048)),
      Var(70052) ~ (Var(121772) union Var(70050)),
      Var(70061) ~ Var(121778),
      Var(70064) ~ Var(121780),
      Var(70067) ~ Var(121782),
      Var(70070) ~ Var(121784),
      Var(70072) ~ Var(121792),
      Var(70074) ~ (Var(121790) union Var(70072)),
      Var(70076) ~ (Var(121788) union Var(70074)),
      Var(70078) ~ (Var(121786) union Var(70076)),
      Var(70088) ~ Var(121794),
      Var(70091) ~ Var(121796),
      Var(70094) ~ Var(121798),
      Var(70097) ~ Var(121800),
      Var(70100) ~ Var(121802),
      Var(70102) ~ Var(121812),
      Var(70104) ~ (Var(121810) union Var(70102)),
      Var(70106) ~ (Var(121808) union Var(70104)),
      Var(70108) ~ (Var(121806) union Var(70106)),
      Var(70110) ~ (Var(121804) union Var(70108)),
      Var(70118) ~ Empty,
      Var(70123) ~ Empty,
      Var(70126) ~ (Var(70118) union Var(70123)),
      Var(70132) ~ Empty,
      Var(70135) ~ Var(70132)
    )
    val s = solveAll(l).get
    // verify(s, l) -- TOO SLOW
  }

  test("Fixpoint.Phase.Compiler.compileRuleIncr") {
    val l = List(
      Cst(14919) ~ (Var(69065) union (Var(69068) union (Var(69076) union (Var(69079) union (Var(69084) union (Var(69091) union (Var(69150) union (Var(69153) union ((Var(69156) union (Var(69160) union Var(69167))) union Var(69174)))))))))),
      Var(69065) ~ Empty,
      Var(69068) ~ Empty,
      Var(69072) ~ Empty,
      Var(69074) ~ Var(121219),
      Var(69076) ~ (Var(121216) union Var(69074)),
      Var(69079) ~ Empty,
      Var(69084) ~ Empty,
      Var(69089) ~ Empty,
      Var(69091) ~ Var(69089),
      Var(69105) ~ Empty,
      Var(69108) ~ Empty,
      Var(69111) ~ Empty,
      Var(69124) ~ Empty,
      Var(69126) ~ Var(69124),
      Var(69128) ~ Var(121244),
      Var(69132) ~ Var(121234),
      Var(69135) ~ (Var(121230) union Var(69132)),
      Var(69138) ~ Var(121252),
      Var(69150) ~ Var(121254),
      Var(69153) ~ Empty,
      Var(69156) ~ Empty,
      Var(69160) ~ Var(121259),
      Var(69165) ~ Var(121263),
      Var(69167) ~ Var(121261),
      Var(69172) ~ Var(121267),
      Var(69174) ~ Var(121265)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Fixpoint.Phase.IndexSelection.queryOp") {
    val l = List(
      Empty ~ ((Var(66558) union (Var(66568) union (Var(66628) union (Var(66633) union (Var(66638) union (Var(66641) union (Var(66644) union (Var(66657) union Var(66660))))))))) union (Var(66668) union (Var(66673) union (Var(66678) union (Var(66684) union Var(66689)))))),
      Var(66556) ~ Empty,
      Var(66558) ~ Var(66556),
      Var(66564) ~ Empty,
      Var(66566) ~ Var(119740),
      Var(66568) ~ (Var(119738) union Var(66566)),
      Var(66582) ~ Empty,
      Var(66597) ~ Var(119746),
      Var(66600) ~ Var(119764),
      Var(66610) ~ Empty,
      Var(66613) ~ Empty,
      Var(66623) ~ Empty,
      Var(66626) ~ Var(119762),
      Var(66628) ~ (Var(119743) union (Var(66597) union Var(66626))),
      Var(66633) ~ Empty,
      Var(66638) ~ Empty,
      Var(66641) ~ Empty,
      Var(66644) ~ Empty,
      Var(66657) ~ Var(119782),
      Var(66660) ~ Empty,
      Var(66668) ~ Empty,
      Var(66673) ~ Empty,
      Var(66678) ~ Empty,
      Var(66684) ~ Empty,
      Var(66689) ~ Empty
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Fixpoint.Phase.Simplifier.simplifyOp") {
    val l = List(
      Empty ~ ((Var(58593) union (Var(58598) union Var(58601))) union (Var(58635) union (Var(58667) union Var(58683)))),
      Var(58568) ~ Empty,
      Var(58576) ~ Empty,
      Var(58579) ~ Empty,
      Var(58583) ~ Var(114603),
      Var(58586) ~ Var(114615),
      Var(58591) ~ Var(114613),
      Var(58593) ~ (Var(114600) union (Var(58583) union Var(58591))),
      Var(58598) ~ Empty,
      Var(58601) ~ Empty,
      Var(58613) ~ Empty,
      Var(58626) ~ Empty,
      Var(58628) ~ Var(114640),
      Var(58630) ~ (Var(114635) union Var(58628)),
      Var(58633) ~ Empty,
      Var(58635) ~ (Var(114631) union Var(58633)),
      Var(58645) ~ Empty,
      Var(58658) ~ Empty,
      Var(58660) ~ Var(114660),
      Var(58662) ~ (Var(114655) union Var(58660)),
      Var(58665) ~ Empty,
      Var(58667) ~ (Var(114651) union Var(58665)),
      Var(58678) ~ Empty,
      Var(58681) ~ Empty,
      Var(58683) ~ (Var(114664) union Var(58681))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Fixpoint.Solver.solveWithStratification") {
    val l = List(
      Empty ~ (Var(65701) union (Var(65724) union Var(65762))),
      Var(65677) ~ Cst(1794221043),
      Var(65683) ~ Empty,
      Var(65686) ~ Empty,
      Var(65688) ~ (Var(119207) union Var(65683)),
      Var(65691) ~ Empty,
      Var(65693) ~ (Var(119204) union Var(65688)),
      Var(65696) ~ Empty,
      Var(65698) ~ (Var(119201) union Var(65693)),
      Var(65701) ~ Var(65717),
      Var(65705) ~ Var(119224),
      Var(65708) ~ Var(119227),
      Var(65710) ~ Var(119226),
      Var(65712) ~ (Var(119221) union (Var(65705) union Var(65710))),
      Var(65715) ~ Var(119230),
      Var(65717) ~ (Var(119218) union Var(65712)),
      Var(65724) ~ (Var(65740) union Var(65758)),
      Var(65729) ~ Var(119240),
      Var(65731) ~ Var(119239),
      Var(65733) ~ (Var(119235) union Var(65731)),
      Var(65736) ~ Var(119245),
      Var(65738) ~ Var(119244),
      Var(65740) ~ (Var(119232) union (Var(65733) union Var(65738))),
      Var(65744) ~ Var(119255),
      Var(65747) ~ Var(119260),
      Var(65749) ~ Var(119259),
      Var(65751) ~ (Var(119257) union Var(65749)),
      Var(65753) ~ (Var(119252) union (Var(65744) union Var(65751))),
      Var(65756) ~ Var(119263),
      Var(65758) ~ (Var(119249) union Var(65753)),
      Var(65762) ~ Empty,
      Var(65765) ~ Cst(1794221043)
    )
    val s = solveAll(l).get
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
      Var(101432) ~ (Cst(101436) union Var(101438)),
      Var(101431) ~ (Var(101433) union Var(101439)),
      (Cst(101436) union Cst(101435)) ~ (Var(101434) union Var(101432))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Iterator.next") {
    val l = List(
      (Cst(1435) union Cst(1436)) ~ Var(55261),
      Var(55251) ~ Var(112576),
      Var(55257) ~ Var(112582),
      Var(55261) ~ Var(112585)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Iterator.toArray") {
    val l = List(
      ((Cst(1500) union Cst(1501)) union Cst(1498)) ~ Var(78914),
      Var(78914) ~ (Var(78917) union (Var(78923) union Var(78926))),
      Var(78917) ~ Var(127244),
      Var(78921) ~ Var(127251),
      Var(78923) ~ ((Var(127248) union Var(127247)) union Var(127249)),
      Var(78926) ~ (Var(127254) union Var(127252))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("List.merge") {
    val l = List(
      Empty ~ Var(86467),
      Var(86412) ~ Empty,
      Var(86416) ~ Empty,
      Var(86420) ~ Empty,
      Var(86423) ~ Var(131895),
      Var(86425) ~ (Var(131893) union Var(86423)),
      Var(86428) ~ Var(131897),
      Var(86430) ~ (Var(131891) union Var(86425)),
      Var(86432) ~ Var(131904),
      Var(86434) ~ (Var(131902) union Var(86432)),
      Var(86437) ~ Var(131906),
      Var(86439) ~ (Var(131900) union Var(86434)),
      Var(86446) ~ Var(131909),
      Var(86453) ~ Var(131912),
      Var(86458) ~ Var(131915),
      Var(86460) ~ Var(131922),
      Var(86462) ~ (Var(131920) union Var(86460)),
      Var(86465) ~ Empty,
      Var(86467) ~ (Var(131918) union Var(86462))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("List.scanRight") {
    val l = List(
      Var(144695) ~ Empty,
      Var(144681) ~ Var(144691),
      Var(144684) ~ Empty,
      Var(144677) ~ Var(144690),
      Var(144680) ~ Var(144696),
      Var(144684) ~ Var(144693),
      Var(144679) ~ Var(144695),
      Var(144682) ~ Var(144691),
      Var(144688) ~ Empty,
      Var(144685) ~ Cst(144689),
      Var(144683) ~ Cst(144689),
      Var(144680) ~ Var(144695),
      Var(144677) ~ Var(144691),
      Empty ~ Empty,
      Empty ~ Empty,
      Empty ~ Empty,
      Empty ~ Empty,
      Empty ~ Empty,
      Empty ~ Empty,
      Var(144691) ~ (Var(144678) union Var(144688)),
      Var(144692) ~ (Var(144682) union Var(144687)),
      Var(144694) ~ (Var(144682) union Var(144687)),
      Var(144685) ~ (Var(144696) union Var(144694)),
      Var(144687) ~ (Var(144679) union Var(144692)),
      Var(144678) ~ (Var(144681) union Var(144686)),
      Var(144686) ~ (Var(144683) union Var(144693))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("MutDeque.sameElements") {
    val l = List(
      (Var(876) union Var(877)) ~ Var(90798),
      Var(90798) ~ (Var(90801) union Var(90804) union Var(90807) union Var(90820) union Var(90823) union Var(90832) union Var(90841) union Var(90844)),
      Var(90801) ~ Var(134687),
      Var(90804) ~ Var(134689),
      Var(90807) ~ Empty,
      Var(90820) ~ Var(134695),
      Var(90823) ~ Var(134697),
      Var(90826) ~ Var(134703),
      Var(90828) ~ Var(134705),
      Var(90830) ~ Var(134707),
      Var(90832) ~ (Var(134701) union Var(134699) union Var(90826) union Var(90828) union Var(90830)),
      Var(90835) ~ Var(134712),
      Var(90837) ~ Var(134714),
      Var(90839) ~ Var(134716),
      Var(90841) ~ (Var(134710) union Var(134708) union Var(90835) union Var(90837) union Var(90839)),
      Var(90844) ~ (Var(134718) union Var(134719))
    )
    val s = solveAll(l).get
    // verify(s, l) -- TOO SLOW
  }

  test("MutDeque.toArray") {
    val l = List(
      (Cst(915) union Cst(913)) ~ (Var(85240) union (Var(85242) union (Var(85244) union (Var(85247) union (Var(85249) union (Var(85252) union (Var(85257) union Var(85270)))))))),
      Var(85240) ~ Var(131193),
      Var(85242) ~ Var(131195),
      Var(85244) ~ Var(131197),
      Var(85247) ~ Empty,
      Var(85249) ~ Var(131199),
      Var(85252) ~ Empty,
      Var(85255) ~ Var(131206),
      Var(85257) ~ ((Var(131204) union Var(131202)) union Var(85255)),
      Var(85261) ~ Var(131215),
      Var(85263) ~ ((Var(131213) union Var(131211)) union Var(85261)),
      Var(85266) ~ Var(131220),
      Var(85268) ~ ((Var(131218) union Var(131216)) union Var(85266)),
      Var(85270) ~ (((Var(131209) union Var(131210)) union Var(131207)) union (Var(85263) union Var(85268)))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("Nec.zipWithA") {
    val l = List(
      Cst(24685) ~ Var(68027),
      Var(67938) ~ Empty,
      Var(67940) ~ (Var(120514) union Var(67938)),
      Var(67943) ~ Empty,
      Var(67945) ~ (Var(120512) union (Var(67940) union Var(67943))),
      Var(67950) ~ Empty,
      Var(67952) ~ Var(120531),
      Var(67954) ~ (Var(120529) union Var(67952)),
      Var(67956) ~ (Var(120524) union Var(67954)),
      Var(67958) ~ (Var(120521) union Var(67956)),
      Var(67960) ~ (Var(120518) union Var(67958)),
      Var(67962) ~ (Var(120510) union Var(67945)),
      Var(67971) ~ Empty,
      Var(67973) ~ Var(120542),
      Var(67975) ~ (Var(120540) union Var(67973)),
      Var(67977) ~ (Var(120535) union Var(67975)),
      Var(67979) ~ (Var(120533) union Var(67977)),
      Var(67988) ~ Empty,
      Var(67990) ~ Var(120553),
      Var(67992) ~ (Var(120551) union Var(67990)),
      Var(67994) ~ (Var(120546) union Var(67992)),
      Var(67996) ~ (Var(120544) union Var(67994)),
      Var(68004) ~ Empty,
      Var(68006) ~ Var(120564),
      Var(68008) ~ (Var(120562) union Var(68006)),
      Var(68010) ~ (Var(120557) union Var(68008)),
      Var(68012) ~ (Var(120555) union Var(68010)),
      Var(68015) ~ Empty,
      Var(68017) ~ (Var(120570) union Var(68015)),
      Var(68020) ~ Empty,
      Var(68022) ~ (Var(120568) union (Var(68017) union Var(68020))),
      Var(68025) ~ Var(120574),
      Var(68027) ~ (Var(120566) union Var(68022))
    )
    val s = solveAll(l).get
    verify(s, l)
  }

  test("RedBlackTree.insertWith") {
    val l = List(
      Cst(39139) ~ Var(73552),
      Var(73498) ~ Var(123880),
      Var(73507) ~ Empty,
      Var(73510) ~ Var(123891),
      Var(73514) ~ Empty,
      Var(73516) ~ (Var(123893) union Var(73514)),
      Var(73518) ~ (Var(123889) union Var(73510)),
      Var(73521) ~ Var(123907),
      Var(73523) ~ (Var(123905) union Var(73521)),
      Var(73525) ~ (Var(123903) union Var(73523)),
      Var(73528) ~ (Var(123899) union Var(73525)),
      Var(73531) ~ Var(123911),
      Var(73535) ~ Empty,
      Var(73537) ~ (Var(123913) union Var(73535)),
      Var(73539) ~ (Var(123909) union Var(73531)),
      Var(73542) ~ Var(123919),
      Var(73545) ~ Var(123925),
      Var(73548) ~ Empty,
      Var(73550) ~ (Var(123923) union Var(73545)),
      Var(73552) ~ Var(73550)
    )
    val s = solveAll(l).get
    verify(s, l)
  }

}
