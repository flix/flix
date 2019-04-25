package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestRedundancy extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  test("HiddenVarSym.Let.01") {
    val input =
      s"""
         |def main(): Int =
         |    let _x = 123;
         |    _x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Lambda.01") {
    val input =
      s"""
         |def main(): Int =
         |    let f = _x -> _x;
         |    f(123)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Match.01") {
    val input =
      s"""
         |def main(): Int =
         |    match (123, 456) with {
         |        case (_x, _y) => _x + _y
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Select.01") {
    val input =
      s"""
         |def main(): Int =
         |    let c = chan Int 1;
         |    select {
         |        case _x <- c => _x
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Existential.01") {
    val input =
      s"""
         |def main(): Bool = \\exists (_x: Int). _x == 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Universal.01") {
    val input =
      s"""
         |def main(): Bool = \\forall (_x: Int). _x == 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("UnusedEnumSym.01") {
    val input =
      s"""
         |enum Color {
         |  case Red,
         |  case Green,
         |  case Blue
         |}
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumSym.02") {
    val input =
      s"""
         |enum One {
         |  case A(Two)
         |}
         |
         |enum Two {
         |  case B(One)
         |}
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumSym.03") {
    val input =
      s"""
         |type USD = USD(Int)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumTag.01") {
    val input =
      s"""
         |enum Color {
         |  case Red,
         |  case Blue
         |}
         |
         |def main(): Color = Red
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("UnusedEnumTag.02") {
    val input =
      s"""
         |enum Color {
         |  case Red,
         |  case Green,
         |  case Blue
         |}
         |
         |def main(): Color = Green
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("UnusedFormalParam.Def.01") {
    val input =
      s"""
         |pub def f(x: Int): Int = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Def.02") {
    val input =
      s"""
         |pub def f(x: Int, y: Int): Int = y
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Def.03") {
    val input =
      s"""
         |pub def f(x: Int, y: Int): Int = x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Def.04") {
    val input =
      s"""
         |pub def f(x: Int, y: Int, z: Int): Int = x + z
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.01") {
    val input =
      s"""
         |pub def f(): Int =
         |  let f = x -> 123;
         |  f(1)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.02") {
    val input =
      s"""
         |pub def f(): Int =
         |  let f = (x, y) -> x;
         |  f(1, 2)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.03") {
    val input =
      s"""
         |pub def f(): Int =
         |  let f = (x, y) -> y;
         |  f(1, 2)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.04") {
    val input =
      s"""
         |pub def f(): Int =
         |  let f = (x, y, z) -> x + z;
         |  f(1, 2, 3)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Forall.01") {
    val input =
      s"""
         |pub def f(): Bool = \\forall(x: Int). true
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Exists.01") {
    val input =
      s"""
         |pub def f(): Bool = \\exists(x: Int). true
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedTypeParam.Def.01") {
    val input =
      s"""
         |pub def f[a](): Int = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.02") {
    val input =
      s"""
         |pub def f[a, b](x: a): a = x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.03") {
    val input =
      s"""
         |pub def f[a, b](x: b): b = x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.04") {
    val input =
      s"""
         |pub def f[a, b, c](x: a, y: c): (a, c) = (x, y)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.01") {
    val input =
      s"""
         |enum Box[a] {
         |    case Box
         |}
         |
         |def main(): Box[Int] = Box
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.02") {
    val input =
      s"""
         |enum Box[a, b] {
         |    case Box(a)
         |}
         |
         |def main(): Box[Int, Int] = Box(123)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.03") {
    val input =
      s"""
         |enum Box[a, b] {
         |    case Box(b)
         |}
         |
         |def main(): Box[Int, Int] = Box(123)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.04") {
    val input =
      s"""
         |enum Box[a, b, c] {
         |    case Box(a, c)
         |}
         |
         |def main(): Box[Int, Int, Int] = Box(123, 456)
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.05") {
    val input =
      s"""
         |enum Box[a, b, c] {
         |    case A(a),
         |    case B(c)
         |}
         |
         |def main(): (Box[Int, Int, Int], Box[Int, Int, Int]) = (A(123), B(456))
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Relation.01") {
    val input =
      s"""
         |rel R[a](x: Int)
         |
         |def main(): Schema { R[Int] } = R(123).
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Relation.02") {
    val input =
      s"""
         |rel R[a, b](x: a)
         |
         |def main(): Schema { R[Int, Int] } = R(123).
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Relation.03") {
    val input =
      s"""
         |rel R[a, b](x: b)
         |
         |def main(): Schema { R[Int, Int] } = R(123).
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Relation.04") {
    val input =
      s"""
         |rel R[a, b, c](x: a, y: c)
         |
         |def main(): Schema { R[Int, Int, Int] } = R(123, 456).
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedVarSym.Let.01") {
    val input =
      s"""
         |pub def f(): Int =
         |  let x = 123;
         |  456
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Let.02") {
    val input =
      s"""
         |pub def f(): Int =
         |  let x = 123;
         |  let y = 456;
         |  x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.01") {
    val input =
      s"""
         |pub def f(): Int =
         |    let (x, y) = (1, 2);
         |    x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.02") {
    val input =
      s"""
         |pub def f(): Int =
         |    let (x, y) = (1, 2);
         |    y
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.03") {
    val input =
      s"""
         |pub def f(): Int =
         |    let (x, y, z) = (1, 2, 3);
         |    x + y
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.04") {
    val input =
      s"""
         |pub def f(): Int =
         |    let (x, y, z) = (1, 2, 3);
         |    x + z
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Pattern.01") {
    val input =
      s"""
         |pub enum Option[t] {
         |    case None,
         |    case Some(t)
         |}
         |
         |pub def f(x: Option[Int]): Int =
         |    match x with {
         |        case y => 123
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Pattern.02") {
    val input =
      s"""
         |pub enum Option[t] {
         |    case None,
         |    case Some(t)
         |}
         |
         |pub def f(x: Option[Int]): Int =
         |    match x with {
         |        case None    => 123
         |        case Some(y) => 456
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Pattern.03") {
    val input =
      s"""
         |pub enum Option[t] {
         |    case None,
         |    case Some(t)
         |}
         |
         |pub def f(x: Option[(Int, Int)]): Int =
         |    match x with {
         |        case None         => 123
         |        case Some((y, z)) => z
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.01") {
    val input =
      s"""
         |def main(): Int =
         |    let c = chan Int 0;
         |    select {
         |        case x <- c => 123
         |    }
         |
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.02") {
    val input =
      s"""
         |def main(): Int =
         |    let c = chan Int 0;
         |    select {
         |        case x <- c => x
         |        case x <- c => 123
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Hole.01") {
    val input =
      s"""
         |pub def f(): Int =
         |    let x = 123;
         |    ?foo
         |
       """.stripMargin
    compile(input, DefaultOptions).get
  }

  test("UnusedVarSym.Hole.02") {
    val input =
      s"""
         |pub def f(): Int =
         |    let (x, y) = (123, 456);
         |    ?foo
         |
       """.stripMargin
    compile(input, DefaultOptions).get
  }

  test("UselessExpression.01") {
    val input =
      s"""
         |def main(): Int = (); 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.02") {
    val input =
      s"""
         |def main(): Int = 123; 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.03") {
    val input =
      s"""
         |def main(): Int =
         |  let x = 123;
         |  456;
         |  x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessPatternMatch.01") {
    val input =
      s"""
         |enum Color {
         |    case Red,
         |    case Blu
         |}
         |
         |def main(): Int =
         |    let c = Red;
         |    match c with {
         |        case Red => 123
         |        case Blu => match c with {
         |            case Red => 123
         |            case Blu => 456
         |        }
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessPatternMatch](result)
  }

  test("UselessPatternMatch.02") {
    val input =
      s"""
         |enum Color {
         |    case Red(Int),
         |    case Blu(Int)
         |}
         |
         |def main(): Int =
         |    let c = Red(123);
         |    match c with {
         |        case Red(_) => 123
         |        case Blu(_) => match c with {
         |            case Red(_) => 123
         |            case Blu(_) => 123
         |        }
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessPatternMatch](result)
  }

  test("UselessPatternMatch.03") {
    val input =
      s"""
         |enum Color {
         |    case Red,
         |    case Blu
         |}
         |
         |enum Shape {
         |    case Circle(Color),
         |    case Square(Color)
         |}
         |
         |def main(): Int =
         |    let s = Circle(Red);
         |    match s with {
         |        case Circle(Red) => 123
         |        case Square(Blu) => match s with {
         |            case Circle(_) => 123
         |            case Square(_) => 456
         |        }
         |        case _ => 789
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessPatternMatch](result)
  }

  test("UselessPatternMatch.04") {
    val input =
      s"""
         |enum Color {
         |    case Red,
         |    case Blu
         |}
         |
         |enum Shape {
         |    case Circle(Color),
         |    case Square(Color)
         |}
         |
         |def main(): Int =
         |    let s = Circle(Red);
         |    match s with {
         |        case Circle(Red) => 123
         |        case Square(Blu) => match s with {
         |            case Square(Red) => 123
         |            case _           => 456
         |        }
         |        case _ => 789
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessPatternMatch](result)
  }

  test("UselessPatternMatch.StablePath.01") {
    val input =
      s"""
         |enum Color {
         |    case Red,
         |    case Blu
         |}
         |
         |def main(): Int =
         |    let r = { c = Red };
         |    match r.c with {
         |        case Red => 123
         |        case Blu => match r.c with {
         |            case Red => 123
         |            case Blu => 456
         |        }
         |    }
         |
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessPatternMatch](result)
  }


}
