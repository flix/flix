package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import org.scalatest.FunSuite

class TestVerifier extends FunSuite {
//
//  test("Sign.flix") {
//    val input =
//      """namespace Sign {
//        |    let Sign<> = (Sign.Bot, Sign.Top, leq, lub, glb);
//        |    lat A(k: Int, v: Sign<>);
//        |
//        |    A(1, Sign.Neg).
//        |    A(2, Sign.Zer).
//        |    A(3, Sign.Pos).
//        |
//        |    A(4, Sign.Neg).
//        |    A(4, Sign.Zer).
//        |    A(4, Sign.Pos).
//        |
//        |    A(5, x) :- A(1, x), A(2, x), A(3, x).
//        |
//        |    A(6, Sign.Zer `plus` Sign.Pos).
//        |    A(7, Sign.Neg `plus` Sign.Pos).
//        |
//        |    A(8, Sign.Zer `times` Sign.Pos).
//        |    A(9, Sign.Neg `times` Sign.Neg).
//        |}
//      """.stripMargin
//
//    val model = new Flix()
//      .addPath("./examples/domains/Belnap.flix")
//      .addPath("./examples/domains/Sign.flix")
//      .addStr(input)
//      .solve()
//      .get
//  }


}
