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
         |def f(): Int =
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
         |def f(): Int =
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
         |def f(): Int =
         |    match (123, 456) {
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
         |def f(): Int & Impure =
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
         |def f(): Bool = exists (_x: Int). _x == 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Universal.01") {
    val input =
      s"""
         |def f(): Bool = forall (_x: Int). _x == 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("ShadowedVar.Def.01") {
    val input =
      """
        |def f(x: Int): Int =
        |    let x = 123;
        |    x
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Def.02") {
    val input =
      """
        |def f(x: Int): Int =
        |    let y = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Let.01") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Let.02") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    let y = 456;
        |    let x = 789;
        |    x
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.01") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    let f = x -> x + 1;
        |    f(x)
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.02") {
    val input =
      """
        |def f(): Int =
        |    let f = x -> {
        |        let x = 456;
        |        x + 1
        |    };
        |    f(123)
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Lambda.03") {
    val input =
      """
        |def f(): Int =
        |    let f = x -> {
        |        let g = x -> 123;
        |        g(456)
        |    };
        |    f(123)
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.01") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    match (456, 789) {
        |        case (x, _) => x
        |    }
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.02") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    match (456, 789) {
        |        case (_, x) => x
        |    }
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.03") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    match (456, 789) {
        |        case (u, v) => u + v
        |        case (x, y) => x + y
        |    }
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Match.04") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    match (456, 789) {
        |        case (u, v) => u + v
        |        case (y, x) => x + y
        |    }
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Existential.01") {
    val input =
      """
        |def f(): Bool =
        |    let x = 123;
        |    exists (x: Int). x == 0
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Universal.01") {
    val input =
      """
        |def f(): Bool =
        |    let x = 123;
        |    forall (x: Int). x == 0
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Select.01") {
    val input =
      """
        |def f(): Int =
        |    let x = 123;
        |    match (456, 789) {
        |        case (u, v) => u + v
        |        case (y, x) => x + y
        |    }
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
  }

  test("ShadowedVar.Select.02") {
    val input =
      """
        |def f(): Int & Impure =
        |    let x = 123;
        |    let c = chan Int 1;
        |    c <- 456;
        |    select {
        |        case y <- c => y
        |        case x <- c => x
        |    }
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.ShadowedVar](result)
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
         |opaque type USD = Int
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
         |def f(): Color = Red
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
         |def f(): Color = Green
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
         |pub def f(): Bool = forall(x: Int). true
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedFormalParam.Exists.01") {
    val input =
      s"""
         |pub def f(): Bool = exists(x: Int). true
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedTypeParam.Def.01") {
    val input =
      s"""
         |pub def f[a: Type](): Int = 123
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.02") {
    val input =
      s"""
         |pub def f[a: Type, b: Type](x: a): a = x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.03") {
    val input =
      s"""
         |pub def f[a: Type, b: Type](x: b): b = x
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Def.04") {
    val input =
      s"""
         |pub def f[a: Type, b: Type, c: Type](x: a, y: c): (a, c) = (x, y)
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
         |    match x {
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
         |    match x {
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
         |    match x {
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
         |def f(): Int & Impure =
         |    let c = chan Int 0;
         |    select {
         |        case x <- c => 123
         |    }
         |
       """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.02") {
    val input =
      s"""
         |def f(): Int & Impure =
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

  test("UnconditionalRecursion.01") {
    val input =
      s"""
         |def f(): Int =
         |    f()
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.02") {
    val input =
      s"""
         |def foo(x: Int, y: Int): Int =
         |    foo(x, y)
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.03") {
    val input =
      s"""
         |def foo(x: Int): Int = match x {
         |    case 0 => foo(999)
         |    case _ => foo(123)
         |}
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.04") {
    val input =
      s"""
         |def foo(x: Int): Int =
         |    if (x == 1)
         |        foo(9)
         |    else
         |        foo(7)
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnconditionalDefRecursion](result)
  }

  test("UnconditionalRecursion.05") {
    val input =
      s"""
         |def bar(_z: Int -> Int): Int =
         |    5
         |
         |def foo(x: Int, y: Int): Int =
         |    bar(foo(x + y))
         |
         |""".stripMargin
    compile(input, DefaultOptions).get
  }

  test("UnconditionalRecursion.06") {
    val input =
      """
        |namespace LL {
        |
        |    pub enum LazyList[t] {
        |        case Empty,
        |        case Cons(t, Lazy[LazyList[t]])
        |    }
        |
        |    pub def constant(x: a): LazyList[a] =
        |        Cons(x, lazy constant(x))
        |
        |}
        |""".stripMargin
      compile(input, DefaultOptions).get
  }

  test("UselessExpression.01") {
    val input =
      s"""
         |def f(): Unit =
         |    123;
         |    ()
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.02") {
    val input =
      s"""
         |def f(): Unit =
         |    21 + 42;
         |    ()
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.03") {
    val input =
      s"""
         |def hof(f: a -> b & e, x: a): b & e = f(x)
         |
         |def f(): Unit =
         |    hof(x -> x + 21, 42);
         |    ()
         |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UnusedFormalParam.Instance.01") {
    val input =
      """
        |lawless class C[a] {
        |    pub def f(x: a): a
        |}
        |
        |instance C[Int] {
        |    pub def f(x: Int): Int = 123
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("RedundantTypeConstraint.Class.01") {
    val input =
      """
        |lawless class C[a]
        |
        |lawless class D[a] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Class.02") {
    val input =
      """
        |lawless class C[a]
        |
        |lawless class D[a] with C[a]
        |
        |lawless class E[a] with C[a], D[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Def.01") {
    val input =
      """
        |lawless class C[a]
        |
        |pub def f(x: a): Bool with C[a], C[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Def.02") {
    val input =
      """
        |lawless class C[a]
        |
        |lawless class D[a] with C[a]
        |
        |pub def f(x: a): Bool with C[a], D[a] = ???
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Sig.01") {
    val input =
      """
        |lawless class C[a]
        |
        |lawless class D[a] {
        |  pub def f(x: a): Bool with C[a], C[a]
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Sig.02") {
    val input =
      """
        |lawless class C[a]
        |
        |lawless class D[a] with C[a]
        |
        |lawless class E[a] {
        |  pub def f(x: a): Bool with C[a], D[a]
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Instance.01") {
    val input =
      """
        |opaque type Box[a] = a
        |
        |lawless class C[a]
        |
        |lawless class D[a]
        |
        |instance D[Box[a]] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("RedundantTypeConstraint.Instance.02") {
    val input =
      """
        |opaque type Box[a] = a
        |
        |lawless class C[a]
        |
        |lawless class D[a] with C[a]
        |
        |lawless class E[a]
        |
        |instance E[Box[a]] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.RedundantTypeConstraint](result)
  }

  test("UnconditionalRecursion.Class.01") {
    val input =
      """
        |pub lawless class C[a] {
        |  pub def f(x: a): String = C.f(x)
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnconditionalSigRecursion](result)
  }

  test("UnusedFormalParam.Class.01") {
    val input =
      """
        |pub lawless class C[a] {
        |  pub def f(x: a): String = "Hello!"
        |}
        |""".stripMargin
    val result = compile(input, DefaultOptions)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

}
