package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.{RedundancyError, TypeError}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestRedundancy extends AnyFunSuite with TestUtils {

  test("DuplicateExtPattern.01") {
    val input =
      s"""
         |def f(): Int32 =
         |    ematch xvar A(123) {
         |        case A(x) => x
         |        case A(x) => x
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.DuplicateExtPattern](result)
  }

  test("DuplicateExtPattern.02") {
    val input =
      s"""
         |def f(): Int32 =
         |    ematch xvar A(123) {
         |        case A(x) => x
         |        case A(_) => x
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.DuplicateExtPattern](result)
  }

  test("DuplicateExtPattern.03") {
    val input =
      s"""
         |def f(): Int32 =
         |    ematch xvar A(123, 456) {
         |        case A(x, _) => x
         |        case A(_, x) => x
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.DuplicateExtPattern](result)
  }

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

  test("HiddenVarSym.ExtMatch.01") {
    val input =
      s"""
         |def f(): Int32 =
         |    ematch xvar A(123) {
         |        case A(_x) => _x
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("HiddenVarSym.Select.01") {
    val input =
      raw"""
           |def f(): Int32 \ Chan = region rc {
           |    let (_, rx) = Channel.buffered(rc, 1);
           |    select {
           |        case _x <- recv(rx) => _x
           |    }
           |}
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[RedundancyError.HiddenVarSym](result)
  }

  test("ShadowedName.Def.01") {
    val input =
      """
        |def f(x: Int32): Int32 =
        |    let x = 123;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Def.02") {
    val input =
      """
        |def f(x: Int32): Int32 =
        |    let y = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Let.01") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    let x = 456;
        |    x
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Let.02") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Lambda.01") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    let f = x -> x;
        |    f(x)
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Lambda.02") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Lambda.03") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Match.01") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Match.02") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Match.03") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Match.04") {
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
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.ExtMatch.01") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    ematch xvar A(456, 789) {
        |        case A(x, _) => x
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.ExtMatch.02") {
    val input =
      """
        |def f(): Int32 =
        |    let x = 123;
        |    ematch xvar A(456, 789) {
        |        case A(_, x) => x
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.ExtMatch.03") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    ematch xvar A(456, 789) {
        |        case A(u, v) => (u, v)
        |        case B(x, y) => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.ExtMatch.04") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    ematch xvar A(456, 789) {
        |        case B(u, v) => (u, v)
        |        case A(y, x) => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Match.05") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    match { x = 456, y = 789 } {
        |        case { x = a, y = b } => (a, b)
        |        case { x, y } => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Match.06") {
    val input =
      """
        |def f(): (Int32, Int32) =
        |    let x = 123;
        |    match { x = { x = 456 }, y = 789 } {
        |        case { x = { x = a }, y = b } => (a, b)
        |        case { x = { x }, y } => (x, y)
        |    }
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Select.01") {
    val input =
      """
        |def f(): Int32 \ Chan = region rc {
        |    let x = 123;
        |    let (tx, rx) = Channel.buffered(rc, 1);
        |    Channel.send(456, tx);
        |    select {
        |        case y <- recv(rx) => y
        |        case x <- recv(rx) => x
        |    }
        |}
        |
      """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Region.01") {
    val input =
      """
        |def f(): Unit = {
        |   region rc {
        |       discard Array#{} @ rc;
        |       region rc {
        |           discard Array#{} @ rc;
        |           ()
        |       }
        |   }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.NewObject.01") {
    val input =
      """
        |import java.lang.Comparable
        |import java.lang.Object
        |
        |def f(): Comparable \ IO =
        |   new Comparable {
        |     def compareTo(x: Object, _y: Object): Int32 =
        |       let x = 0;
        |       x
        |   }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Trait.01") {
    val input =
      """
        |trait C[a] {
        |    pub def f[a: Type](x: a): Int32
        |}
        |
        |def f(): Unit = ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.01") {
    val input =
      s"""
         |mod Foo {
         |    pub def f(): Unit = ()
         |}
         |
         |def foo(): Bool =
         |    use Foo.f;
         |    let f = _ -> true;
         |    f(123)
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.02") {
    val input =
      s"""
         |mod Foo {
         |    pub def f(): Unit = ()
         |    pub def g(): Unit = ()
         |}
         |
         |def foo(): Bool =
         |    use Foo.f;
         |    let f = _ -> true;
         |    use Foo.g;
         |    let g = _ -> true;
         |    f(g(123))
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.03") {
    val input =
      s"""
         |def foo(): Bool =
         |    use A.f;
         |    use B.f;
         |    f() == f()
         |
         |mod A {
         |    pub def f(): Int32 = 1
         |}
         |
         |mod B {
         |    pub def f(): Int32 = 1
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.11") {
    val input =
      s"""
         |def foo(): Bool =
         |    use A.Color;
         |    use B.Color;
         |    true
         |
         |mod A {
         |    enum Color {
         |        case Red, Blue
         |    }
         |}
         |
         |mod B {
         |    enum Color {
         |        case Red, Blue
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.14") {
    val input =
      s"""
         |def foo(): Bool =
         |    use A.Color.Red;
         |    use B.Color.Red;
         |    Red == Red
         |
         |mod A {
         |    pub enum Color with Eq {
         |        case Red, Blu
         |    }
         |}
         |
         |mod B {
         |    pub enum Color with Eq {
         |        case Red, Blu
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.17") {
    val input =
      s"""
         |def foo(): Bool =
         |    use A.Color.{Red => R};
         |    use A.Color.{Blu => R};
         |    R == R
         |
         |mod A {
         |    pub enum Color with Eq {
         |        case Red, Blu
         |    }
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  // TODO NS-REFACTOR redundancy on top-level uses
  test("ShadowedName.Use.18") {
    val input =
      s"""
         |mod T {
         |    use A.Color.Red
         |    use B.Color.Red
         |    def foo(): Bool =
         |        Red == Red
         |}
         |
         |def foo(): Bool =
         |    use A.Color.Red;
         |    use B.Color.Red;
         |    Red == Red
         |
         |mod A {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
         |
         |mod B {
         |    enum Color {
         |        case Red, Blu
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedName.Use.21") {
    val input =
      s"""
         |def foo(): Bool =
         |    use A.Color;
         |    use B.Color;
         |    true
         |
         |mod A {
         |    struct Color[r] {
         |    }
         |}
         |
         |mod B {
         |    struct Color[r] {
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("UnreachableExtPattern.01") {
    val input =
      """
        |def f(): Bool = ematch A() {
        |    case _   => true
        |    case A() => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnreachableExtMatchCase](result)
  }

  test("UnreachableExtPattern.02") {
    val input =
      """
        |def f(): Bool = ematch A() {
        |    case B() => false
        |    case _   => true
        |    case A() => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnreachableExtMatchCase](result)
  }

  test("UnreachableExtPattern.03") {
    val input =
      """
        |def f(): Bool = ematch B() {
        |    case A() => false
        |    case A() => false
        |    case _   => true
        |    case B() => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnreachableExtMatchCase](result)
    expectError[RedundancyError.DuplicateExtPattern](result)
  }

  test("UnreachableExtPattern.04") {
    val input =
      """
        |def f(): Bool = ematch B() {
        |    case A() => false
        |    case _   => true
        |    case _   => false
        |    case B() => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnreachableExtMatchCase](result)
  }

  test("UnreachableExtPattern.05") {
    val input =
      """
        |def f(): Bool = ematch C() {
        |    case A() => false
        |    case _   => true
        |    case _   => false
        |    case B() => false
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnreachableExtMatchCase](result)
  }

  test("UnusedStructSym.01") {
    val input =
      s"""
         |mod N {
         |    struct Color[r] {
         |    }
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedStructSym](result)
  }

  test("UnusedStructSym.02") {
    val input =
      s"""
         |mod N {
         |    struct One[r] {
         |        a: Two[r]
         |    }
         |
         |    struct Two[r] {
         |        b: One[r]
         |    }
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedStructSym](result)
  }

  test("UnusedEnumSym.01") {
    val input =
      s"""
         |mod N {
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
         |mod N {
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
         |mod N {
         |    enum USD(Int32)
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumSym](result)
  }

  test("UnusedEnumTag.01") {
    val input =
      s"""
         |mod N {
         |    enum Color {
         |        case Red,
         |        case Blue
         |    }
         |
         |    def f(): Color = Color.Red
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("UnusedEnumTag.02") {
    val input =
      s"""
         |mod N {
         |    enum Color {
         |        case Red,
         |        case Green,
         |        case Blue
         |    }
         |
         |    def f(): Color = Color.Green
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEnumTag](result)
  }

  test("PrefixedStructSym.01") {
    val input =
      s"""
         |mod N {
         |    struct _Color[r] { }
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("PrefixedEnumSym.01") {
    val input =
      s"""
         |mod N {
         |    enum _Color {
         |        case Red,
         |        case Green,
         |        case Blue
         |    }
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
  }

  test("PrefixedEnumSym.02") {
    val input =
      s"""
         |mod N {
         |    enum _USD(Int32)
         |}
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectSuccess(result)
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

  test("UnusedFormalParam.NewObject.01") {
    val input =
      """
        |import java.lang.Comparable
        |import java.lang.Object
        |
        |def f(): Comparable \ IO =
        |   new Comparable {
        |     def compareTo(x: Object, _y: Object): Int32 =
        |       0
        |   }
        """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
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

  test("UnusedTypeParam.Struct.01") {
    val input =
      s"""
         |struct Box[a, r] { }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Struct.02") {
    val input =
      s"""
         |struct Box[a, b, r] {
         |    box: b
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Struct.03") {
    val input =
      s"""
         |struct Box[a, b, r] {
         |    box: b
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Struct.04") {
    val input =
      s"""
         |struct Box[a, b, c, r] {
         |    box: (a, c)
         |}
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedTypeParam](result)
  }

  test("UnusedTypeParam.Struct.05") {
    val input =
      s"""
         |enum Box[a, b, c, r] {
         |    f1: a
         |    f2: b
         |}
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

  test("UnusedTypeParam.TypeAlias.01") {
    val input =
      """
        |type alias T[a] = Int32
        |""".stripMargin
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

  test("UnusedVarSym.Pattern.04") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    match { x = 1 } {
         |        case { x } => 42
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Pattern.05") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    match { x = 1 } {
         |        case { x = y } => 42
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.ExtPattern.01") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    ematch xvar A(1) {
         |        case A(x) => 42
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.ExtPattern.02") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    ematch xvar AB(1, 2, 3) {
         |        case AB(x, y, z) => 42
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.ExtPattern.03") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    ematch xvar AB(1, 2, 3) {
         |        case A(x) => 42
         |        case AB(x, y, z) => 42
         |    }
         |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.01") {
    val input =
      raw"""
           |def f(): Int32 = region rc {
           |    let (_, rx) = Channel.unbuffered(rc);
           |    select {
           |        case x <- recv(rx) => 123
           |    }
           |}
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("UnusedVarSym.Select.02") {
    val input =
      raw"""
           |def f(): Int32 = region rc {
           |    let (_, rx) = Channel.unbuffered(rc);
           |    select {
           |        case x <- recv(rx) => x
           |        case x <- recv(rx) => 123
           |    }
           |}
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibAll)
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
    compile(input, Options.TestWithLibNix).unsafeGet
  }

  test("UnusedVarSym.Hole.02") {
    val input =
      s"""
         |pub def f(): Int32 =
         |    let (x, y) = (123, 456);
         |    ?foo
         |
       """.stripMargin
    compile(input, Options.TestWithLibNix).unsafeGet
  }

  test("UnusedVarSym.PreviousError.01") {
    val input =
      """
        |pub def f(): Int32 =
        |    let x = 123;
        |    "hello"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("UnusedVarSym.PreviousError.02") {
    val input =
      """
        |pub def f(x: Int32): Int32 =
        |    "hello"
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[TypeError](result)
  }

  test("UselessExpression.01") {
    val input =
      s"""
         |def f(): Unit =
         |    ();
         |    ()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.02") {
    val input =
      s"""
         |def foo(): Unit = ()
         |def f(): Unit =
         |    foo();
         |    ()
         |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.03") {
    val input =
      """
        |def hof(f: a -> Unit \ e, x: a): Unit \ e = f(x)
        |
        |def f(): Unit =
        |    hof(x -> (), 42);
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UselessExpression.04") {
    val input =
      """
        |def f(): Unit =
        |    discard 123;
        |    ()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessExpression](result)
  }

  test("UnusedFormalParam.Instance.01") {
    val input =
      """
        |trait C[a] {
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
      """
        |pub def f(): Int32 = unchecked_cast(123 as _ \ {})
        |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantUncheckedEffectCast](result)
  }

  test("RedundantPurityCast.02") {
    val input =
      raw"""
           |pub def f(): Array[Int32, Static] \ IO =
           |  let x = Array#{1, 2, 3} @ Static;
           |  unchecked_cast(x as _ \ {})
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.RedundantUncheckedEffectCast](result)
  }

  test("RedundantUncheckedEffectCast.01") {
    val input =
      raw"""
           |pub def f(g: Int32 -> Int32 \ ef): Int32 \ ef = unchecked_cast(g(123) as _ \ ef)
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantUncheckedEffectCast](result)
  }

  test("UselessUnsafe.01") {
    val input =
      raw"""
           |pub def f(): Int32 = unsafely {} run 42
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UselessUnsafe](result)
  }

  test("RedundantUnsafe.01") {
    val input =
      raw"""
           |pub def f(): Int32 = unsafely IO run 42
           |
       """.stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantUnsafe](result)
  }

  test("RedundantTraitConstraint.Trait.01") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Trait.02") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a] with C[a]
        |
        |trait E[a] with C[a], D[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Def.01") {
    val input =
      """
        |trait C[a]
        |
        |pub def f(x: a): Bool with C[a], C[a] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Def.02") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a] with C[a]
        |
        |pub def f(x: a): Bool with C[a], D[a] = ???
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Sig.01") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a] {
        |  pub def f(x: a): Bool with C[a], C[a]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Sig.02") {
    val input =
      """
        |trait C[a]
        |
        |trait D[a] with C[a]
        |
        |trait E[a] {
        |  pub def f(x: a): Bool with C[a], D[a]
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Instance.01") {
    val input =
      """
        |enum Box[a](a)
        |
        |trait C[a]
        |
        |trait D[a]
        |
        |instance D[Box[a]] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("RedundantTraitConstraint.Instance.02") {
    val input =
      """
        |enum Box[a](a)
        |
        |trait C[a]
        |
        |trait D[a] with C[a]
        |
        |trait E[a]
        |
        |instance E[Box[a]] with C[a], C[a]
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantTraitConstraint](result)
  }

  test("UnusedFormalParam.Trait.01") {
    val input =
      """
        |pub trait C[a] {
        |  pub def f(x: a): String = "Hello!"
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedFormalParam](result)
  }

  test("UnusedDefSym.01") {
    val input =
      """
        |mod N {
        |    def foo(): Bool = true
        |}
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedDefSym](result)
  }

  test("UnusedDefSym.Recursive.01") {
    val input =
      """
        |mod N {
        |    def foo(): Bool = if (true) foo() else false
        |}
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedDefSym](result)
  }

  test("UnusedEffSym.01") {
    val input =
      """
        |mod N {
        |    eff E
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEffSym](result)
  }

  test("UnusedEffSym.02") {
    val input =
      """
        |mod N {
        |    eff E
        |    def foo(): Unit \ E = unchecked_cast(??? as _ \ E)
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedEffSym](result)
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
        |def fakePrint(_msg: a): Unit \ IO =
        |    discard Array#{2} @ Static;
        |    ()
        |
        |def f(g: a -> b \ ef, x: a): b \ ef = g(x)
        |
        |def h(): Unit \ IO = f(fakePrint, discard "hello")
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.DiscardedPureValue](result)
  }

  test("RedundantDiscard.01") {
    val input =
      """
        |def fakePrint(_msg: a): Unit \ IO =
        |    discard Array#{2} @ Static;
        |    ()
        |
        |def f(): Unit \ IO =
        |    discard fakePrint("hello")
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.RedundantDiscard](result)
  }

  test("RedundantDiscard.02") {
    val input =
      """
        |def f(g: a -> b \ ef, x: a): b \ ef = g(x)
        |
        |def h(): Unit \ IO =
        |    let arr = Array#{()} @ Static;
        |    discard f((i: Int32) -> %%ARRAY_LOAD%%(arr, i), 0)
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.RedundantDiscard](result)
  }

  test("RedundantCheckedTypeCast.01") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            checked_cast(x -> x + 1)
        |        else
        |            x -> x + 1;
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.RedundantCheckedTypeCast](result)
  }

  test("RedundantCheckedTypeCast.02") {
    val input =
      """
        |def f(): Unit \ IO =
        |    let _ =
        |        if (true)
        |            checked_cast(x -> x + 1)
        |        else {
        |            println(1);
        |            x -> x + 1
        |        };
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibMin)
    expectError[RedundancyError.RedundantCheckedTypeCast](result)
  }

  test("RedundantCheckedTypeCast.03") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            checked_cast((1, "a"))
        |        else
        |            (1, "a");
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantCheckedTypeCast](result)
  }

  test("RedundantCheckedTypeCast.04") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> unchecked_cast(() as _ \ { A, B, C });
        |    let g = () -> unchecked_cast(() as _ \ { A, B, C });
        |    let _ =
        |        if (true)
        |            checked_cast(f)
        |        else
        |            g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantCheckedTypeCast](result)
  }

  test("RedundantCheckedEffectCast.01") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            checked_ecast(x -> x)
        |        else
        |            x -> x;
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantCheckedEffectCast](result)
  }

  test("RedundantCheckedEffectCast.02") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            checked_ecast(())
        |        else
        |            region rc {
        |                let _ = %%ARRAY_NEW%%(rc, 8, 8);
        |                ()
        |            };
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantCheckedEffectCast](result)
  }

  test("RedundantCheckedEffectCast.03") {
    val input =
      """
        |def f(): Unit =
        |    let _ =
        |        if (true)
        |            checked_ecast((1, "a"))
        |        else
        |            (1, "a");
        |    ()
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantCheckedEffectCast](result)
  }

  test("RedundantCheckedEffectCast.05") {
    val input =
      """
        |pub eff A
        |pub eff B
        |pub eff C
        |
        |def f(): Unit =
        |    let f = () -> unchecked_cast(() as _ \ { A, B, C });
        |    let g = () -> unchecked_cast(() as _ \ { A, B, C });
        |    let _ =
        |        if (true)
        |            checked_ecast(f)
        |        else
        |            g;
        |    ()
        |
        |""".stripMargin

    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.RedundantCheckedEffectCast](result)
  }

  test("TestParYield.01") {
    val input =
      """
        |def f(): Int32 =
        |    let g = () -> 1;
        |    par (g <- 5) yield g
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("TestParYield.02") {
    val input =
      """
        |def f(): Int32 =
        |    par (a <- 5; a <- 1) yield a
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("TestParYield.03") {
    val input =
      """
        |def f(): Int32 =
        |    let a = 1;
        |    par (a <- 5) yield a
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("TestParYield.04") {
    val input =
      """
        |def f(): Int32 =
        |    par (a <- 5) yield { let a = 4; a }
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowedName](result)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("TestParYield.05") {
    val input =
      """
        |def f(): Int32 =
        |    par (a <- 5) yield 1
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.UnusedVarSym](result)
  }

  test("ShadowedVariable.LocalDef.01") {
    val input =
      """
        |def f(): Int32 =
        |   def g() = { def g() = 1; g() };
        |   g()
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedVariable.Handler.01") {
    val input =
      """
        |eff E {
        |    def op(x: String): String
        |}
        |
        |def foo(arg: String): String = {
        |    run ??? with handler E {
        |        def op(arg, k) = ???
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("ShadowedVariable.Handler.02") {
    val input =
      """
        |eff E {
        |    def op(x: String): String
        |}
        |
        |def foo(arg: String): String = {
        |    run ??? with handler E {
        |        def op(arg, k) = {
        |            let k = "";
        |            k
        |        }
        |    }
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibNix)
    expectError[RedundancyError.ShadowingName](result)
  }

  test("VariableInLongListLiteral.01") {
    val input =
      """
        |def f(): List[Int32] = {
        |    let xs = 25 :: 26 :: 27 :: Nil;
        |    let l = 1 :: 2 :: 3 :: 4 :: 5 :: 6 :: 7 :: 8 :: 9 :: 10 :: 11 :: 12 :: 13
        |              :: 14 :: 15 :: 16 :: 17 :: 18 :: 19 :: 20 :: 21 :: 22 :: 23 :: 24 :: xs;
        |    l
        |}
        |""".stripMargin
    val result = compile(input, Options.TestWithLibAll)
    expectSuccess(result)
  }
}
