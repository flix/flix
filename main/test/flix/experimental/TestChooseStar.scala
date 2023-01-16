package flix.experimental

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TypeError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestChooseStar extends FunSuite with TestUtils {
  test("TestChooseStar.01") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedBools](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.02") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def quack(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedBools](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.03") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def liquorice(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Not(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedBools](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.04") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def testChooseStar4(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Cst(_) => Expr.Var()
        |        case Expr.Not(_) => Expr.Var()
        |        case Expr.Xor(_) => Expr.Not()
        |    };
        |    choose star {
        |        case Expr.Var(_) => true
        |        case Expr.Xor(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedBools](compile(input, Options.TestWithLibNix))
  }

  test("TestChooseStar.05") {
    val input =
      """
        |restrictable enum Expr[s] {
        |    case Cst, Var, Not, And, Or, Xor
        |}
        |
        |pub def foo(): Bool = {
        |    // P2: check the lower bound by using result in a choose
        |    let star = choose* Expr.Cst {
        |        case Expr.Not(_) => Expr.Not()
        |        case Expr.Cst(_) => Expr.Var()
        |    };
        |    choose star {
        |        case Expr.Cst(_) => false
        |    }
        |}
        |""".stripMargin
    expectError[TypeError.MismatchedBools](compile(input, Options.TestWithLibNix))
  }
}
