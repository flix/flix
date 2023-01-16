package flix.experimental

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestChoose extends FunSuite with TestUtils {
  test("TestChoose.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = choose Expr.Cst {
        |    case Expr.Var(_) => true
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedBools](compile(input, Options.TestWithLibNix))
  }

  test("TestChoose.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |     };
        |     let h = if (true) f else g;
        |     h(Expr.Var)
        | }
        |""".stripMargin
    expectError[TypeError.MismatchedArrowBools](compile(input, Options.TestWithLibNix))
  }

  test("TestChoose.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        | pub def testChoose06(): Bool = {
        |     let f = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Var(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let g = x -> choose x {
        |         case Expr.Cst(_) => false
        |         case Expr.Xor(_) => true
        |         case Expr.Not(_) => false
        |     };
        |     let h = if (true) f else g;
        |
        |     let cstOrNotOrVar = if (true) Expr.Cst else if (true) Expr.Not else Expr.Var;
        |
        |     h(cstOrNotOrVar)
        | }
        |""".stripMargin
    expectError[TypeError.MismatchedArrowBools](compile(input, Options.TestWithLibNix))
  }
}
