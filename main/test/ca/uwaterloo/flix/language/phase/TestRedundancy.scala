package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.RedundancyError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestRedundancy extends FunSuite with TestUtils {

  test("HiddenVarSym.Let.01") {
    val input =
      s"""
         |def f(): Int32 =
         |    let _x = 123;
         |    _x
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Lambda.01") {
    val input =
      s"""
         |def f(): Int32 =
         |    let f = _x -> _x;
         |    f(123)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Match.01") {
    val input =
      s"""
         |def f(): (Int32, Int32) =
         |    match (123, 456) {
         |        case (_x, _y) => (_x, _y)
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Select.01") {
    val input =
      s"""
         |def f(): Int32 & Impure =
         |    let c = chan Int32 1;
         |    select {
         |        case _x <- c => _x
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Predicate.01") {
    val input =
      s"""
         |def f(): #{ A(Int32), B(Int32) } =
         |  #{
         |    A(_x) :- B(_x).
         |  }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Predicate.02") {
    val input =
      s"""
         |def f(): #{ A(Int32), B(Int32), C(Int32) } =
         |  #{
         |    A(2) :- B(_x), C(_x).
         |  }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("ShadowedVar.Def.01") {
    val input =
      """
        |def f(x: Int32): Int32 =
        |    let x = 123;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Def.02") {
    val input =
      """
        |def f(x: Int32): Int32 =
        |    let y = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Let.01") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Let.02") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    let y = 456;
        |    let x = 789;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.01") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    let f = x -> x;
        |    f(x)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.02") {
    val input =
      """
        |def f(): Int32 =
        |    let f = x -> {
        |        let x = 456;
        |        x
        |    };
        |    f(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.03") {
    val input =
      """
        |def f(): Int32 =
        |    let f = x -> {
        |        let g = x -> 123;
        |        g(456)
        |    };
        |    f(123)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.01") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    match (456, 789) {
        |        case (x, _) => x
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.02") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    match (456, 789) {
        |        case (_, x) => x
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.03") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    match (456, 789) {
        |        case (u, v) => (u, v)
        |        case (x, y) => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.04") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    match (456, 789) {
        |        case (u, v) => (u, v)
        |        case (y, x) => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Select.01") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    match (456, 789) {
        |        case (u, v) => (u, v)
        |        case (y, x) => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Select.02") {
    val input =
      """
        |def f(): Int32 & Impure =
        |    let x = 123;
        |    let c = chan Int32 1;
        |    c <- 456;
        |    select {
        |        case y <- c => y
        |        case x <- c => x
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Region.01") {
    val input =
      """
        |def f(): Unit = {
        |   region r {
        |       let _ = [] @ r;
        |       region r {
        |           let _ = [] @ r;
        |           ()
        |       }
        |   }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("UnusedEnumSym.01") {
    val input =
      s"""
         |namespace N {
         |    enum Color {
         |        case Red,
         |        case Green,
         |        case Blue
         |    }
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumSym.02") {
    val input =
      s"""
         |namespace N {
         |    enum One {
         |      case A(Two)
         |    }
         |
         |    enum Two {
         |      case B(One)
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumSym.03") {
    val input =
      s"""
         |namespace N {
         |    enum USD(Int32)
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumTag.01") {
    val input =
      s"""
         |namespace N {
         |    enum Color {
         |        case Red,
         |        case Blue
         |    }
         |
         |    def f(): Color = Red
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("UnusedEnumTag.02") {
    val input =
      s"""
         |namespace N {
         |    enum Color {
         |        case Red,
         |        case Green,
         |        case Blue
         |    }
         |
         |    def f(): Color = Green
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("UnusedFormalParam.Def.01") {
    val input =
      s"""
         |pub def f(x: Int32): Int32 = 123
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Def.02") {
    val input =
      s"""
         |pub def f(x: Int32, y: Int32): Int32 = y
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Def.03") {
    val input =
      s"""
         |pub def f(x: Int32, y: Int32): Int32 = x
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Def.04") {
    val input =
      s"""
         |pub def f(x: Int32, y: Int32, z: Int32): (Int32, Int32) = (x, z)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.01") {
    val input =
      s"""
         |pub def f(): Int32 =
         |  let f = x -> 123;
         |  f(1)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.02") {
    val input =
      s"""
         |pub def f(): Int32 =
         |  let f = (x, y) -> x;
         |  f(1, 2)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.03") {
    val input =
      s"""
         |pub def f(): Int32 =
         |  let f = (x, y) -> y;
         |  f(1, 2)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Lambda.04") {
    val input =
      s"""
         |pub def f(): (Int32, Int32) =
         |  let f = (x, y, z) -> (x, z);
         |  f(1, 2, 3)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedTypeParam.Def.01") {
    val input =
      s"""
         |pub def f[a: Type](): Int32 = 123
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.02") {
    val input =
      s"""
         |pub def f[a: Type, b: Type](x: a): a = x
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.03") {
    val input =
      s"""
         |pub def f[a: Type, b: Type](x: b): b = x
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.04") {
    val input =
      s"""
         |pub def f[a: Type, b: Type, c: Type](x: a, y: c): (a, c) = (x, y)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.01") {
    val input =
      s"""
         |enum Box[a] {
         |    case Box
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.02") {
    val input =
      s"""
         |enum Box[a, b] {
         |    case Box(a)
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.03") {
    val input =
      s"""
         |enum Box[a, b] {
         |    case Box(b)
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Enum.04") {
    val input =
      s"""
         |enum Box[a, b, c] {
         |    case Box(a, c)
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
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
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedVarSym.Let.01") {
    val input =
      s"""
         |pub def f(): Int32 =
         |  let x = 123;
         |  456
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Let.02") {
    val input =
      s"""
         |pub def f(): Int32 =
         |  let x = 123;
         |  let y = 456;
         |  x
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.01") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    let (x, y) = (1, 2);
         |    x
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.02") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    let (x, y) = (1, 2);
         |    y
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.03") {
    val input =
      s"""
         |pub def f(): (Int32, Int32) =
         |    let (x, y, z) = (1, 2, 3);
         |    (x, y)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetMatch.04") {
    val input =
      s"""
         |pub def f(): (Int32, Int32) =
         |    let (x, y, z) = (1, 2, 3);
         |    (x, z)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetRec.01") {
    val input =
      """
        |pub def f(): Bool =
        |    def g() = if (true) g() else false;
        |    true
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.LetRec.02") {
    val input =
      """
        |pub def f(): Bool =
        |    def g() = {
        |        def h() = {
        |            if (true) g() else h()
        |        };
        |        if (true) g() else h()
        |    };
        |    true
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
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
         |pub def f(x: Option[Int32]): Int32 =
         |    match x {
         |        case y => 123
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
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
         |pub def f(x: Option[Int32]): Int32 =
         |    match x {
         |        case None    => 123
         |        case Some(y) => 456
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
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
         |pub def f(x: Option[(Int32, Int32)]): Int32 =
         |    match x {
         |        case None         => 123
         |        case Some((y, z)) => z
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.01") {
    val input =
      s"""
         |def f(): Int32 & Impure =
         |    let c = chan Int32 0;
         |    select {
         |        case x <- c => 123
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.02") {
    val input =
      s"""
         |def f(): Int32 & Impure =
         |    let c = chan Int32 0;
         |    select {
         |        case x <- c => x
         |        case x <- c => 123
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Hole.01") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    let x = 123;
         |    ?foo
         |
       """.stripMargin
    compile(input, Options.TestWithLibNix).get
  }

  test("UnusedVarSym.Hole.02") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    let (x, y) = (123, 456);
         |    ?foo
         |
       """.stripMargin
    compile(input, Options.TestWithLibNix).get
  }

  test("UselessExpression.01") {
    val input =
      s"""
         |def f(): Unit =
         |    123;
         |    ()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.02") {
    val input =
      s"""
         |def f(): Unit =
         |    (21, 42);
         |    ()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.03") {
    val input =
      s"""
         |def hof(f: a -> b & e, x: a): b & e = f(x)
         |
         |def f(): Unit =
         |    hof(x -> (x, 21), 42);
         |    ()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UnusedFormalParam.Instance.01") {
    val input =
      """
        |class C[a] {
        |    pub def f(x: a): a
        |}
        |
        |instance C[Int32] {
        |    pub def f(x: Int32): Int32 = 123
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("RedundantPurityCast.01") {
    val input =
      s"""
         |pub def f(): Int32 = 123 as & Pure
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantPurityCast](result)
  }

  test("RedundantPurityCast.02") {
    val input =
      s"""
         |pub def f(): Array[Int32, false] & Impure =
         |  let x = [1, 2, 3];
         |  x as & Pure
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.RedundantPurityCast](result)
  }

  test("RedundantEffectCast.01") {
    val input =
      s"""
         |pub def f(g: Int32 -> Int32 & ef): Int32 & ef = g(123) as & ef
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantEffectCast](result)
  }

  test("RedundantTypeConstraint.Class.01") {
    val input =
      """
        |class C[a]
        |
        |class D[a] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Class.02") {
    val input =
      """
        |class C[a]
        |
        |class D[a] with C[a]
        |
        |class E[a] with C[a], D[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Def.01") {
    val input =
      """
        |class C[a]
        |
        |pub def f(x: a): Bool with C[a], C[a] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Def.02") {
    val input =
      """
        |class C[a]
        |
        |class D[a] with C[a]
        |
        |pub def f(x: a): Bool with C[a], D[a] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Sig.01") {
    val input =
      """
        |class C[a]
        |
        |class D[a] {
        |  pub def f(x: a): Bool with C[a], C[a]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Sig.02") {
    val input =
      """
        |class C[a]
        |
        |class D[a] with C[a]
        |
        |class E[a] {
        |  pub def f(x: a): Bool with C[a], D[a]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Instance.01") {
    val input =
      """
        |enum Box[a](a)
        |
        |class C[a]
        |
        |class D[a]
        |
        |instance D[Box[a]] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Instance.02") {
    val input =
      """
        |enum Box[a](a)
        |
        |class C[a]
        |
        |class D[a] with C[a]
        |
        |class E[a]
        |
        |instance E[Box[a]] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("UnusedFormalParam.Class.01") {
    val input =
      """
        |pub class C[a] {
        |  pub def f(x: a): String = "Hello!"
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("IllegalSingleVariable.Predicate.01") {
    val input =
      s"""
         |namespace N {
         |    def f(): #{ A(Int32), B(Int32), C(Int32) } =
         |        #{
         |            A(x) :- B(x), C(y).
         |        }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.IllegalSingleVariable](result)
  }

  test("UnusedDefSym.01") {
    val input =
      """
        |namespace N {
        |    def foo(): Bool = true
        |}
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedDefSym](result)
  }

  test("UnusedDefSym.Recursive.01") {
    val input =
      """
        |namespace N {
        |    def foo(): Bool = if (true) foo() else false
        |}
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedDefSym](result)
  }

  test("DiscardedPureValue.01") {
    val input =
      """
        |def f(): Unit =
        |    let x = discard 2;
        |    x
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.DiscardedPureValue](result)
  }

  test("DiscardedPureValue.02") {
    val input =
      """
        |def fakePrint(_msg: a): Unit & Impure =
        |    let _ = [2];
        |    ()
        |
        |def f(g: a -> b & ef, x: a): b & ef = g(x)
        |
        |def h(): Unit & Impure = f(fakePrint, discard "hello")
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.DiscardedPureValue](result)
  }

  test("RedundantDiscard.01") {
    val input =
      """
        |def fakePrint(_msg: a): Unit & Impure =
        |    let _ = [2];
        |    ()
        |
        |def f(): Unit & Impure =
        |    discard fakePrint("hello")
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantDiscard](result)
  }

  test("RedundantDiscard.02") {
    val input =
      """
        |def f(g: a -> b & ef, x: a): b & ef = g(x)
        |
        |def h(): Unit & Impure =
        |    let arr = [()];
        |    discard f((i: Int32) -> arr[i], 0)
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantDiscard](result)
  }
}
