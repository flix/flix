package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TerminationError.{ForbiddenExpression, NonStrictlyPositiveType, NonStructuralRecursion}
import ca.uwaterloo.flix.util.Options
import org.scalatest.funsuite.AnyFunSuite

class TestTerminator extends AnyFunSuite with TestUtils {

  // =========================================================================
  // NonStructuralRecursion
  // =========================================================================

  test("NonStructuralRecursion.01") {
    // Recursive call with unchanged argument: f(x) calls f(x)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 = f(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.02") {
    // Recursive call with a completely unrelated argument (wrapping in Cons)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, xs) => f(MyList.Cons(1, xs))
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.03") {
    // Recursive call outside of any pattern match
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 =
        |    if (true) 0 else f(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.04") {
    // Recursive call uses the whole scrutinee via top-level Var, not a sub-pattern
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil => 0
        |    case y          => f(y)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.05") {
    // Tuple scrutinee but recursive call passes original params, not sub-patterns
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(l1: MyList[Int32], l2: MyList[Int32]): Int32 = match (l1, l2) {
        |    case (MyList.Nil, _)                          => 0
        |    case (MyList.Cons(_, xs), MyList.Cons(_, ys)) => f(l1, l2)
        |    case _                                         => 0
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.06") {
    // Tuple scrutinee: passes a sub of l1 but in l2's position, and l1's position gets l2
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(l1: MyList[Int32], l2: MyList[Int32]): Int32 = match (l1, l2) {
        |    case (MyList.Nil, _)                          => 0
        |    case (MyList.Cons(_, xs), MyList.Cons(_, ys)) => f(l2, xs)
        |    case _                                         => 0
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.07") {
    // Recursive call with a variable not derived from any formal parameter
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, _)  =>
        |        let y = MyList.Nil;
        |        f(y)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  // =========================================================================
  // NonStrictlyPositiveType
  // =========================================================================

  test("NonStrictlyPositiveType.01") {
    // Enum with recursive type in negative position (left of arrow)
    val input =
      """
        |enum Bad { case MkBad(Bad -> Int32) }
        |@Terminates
        |def f(x: Bad): Int32 = match x {
        |    case Bad.MkBad(_) => 0
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStrictlyPositiveType](result)
  }

  test("NonStrictlyPositiveType.02") {
    // Polymorphic enum with type param in negative position
    val input =
      """
        |enum Neg[a] { case MkNeg(Neg[a] -> a) }
        |@Terminates
        |def f(x: Neg[Int32]): Int32 = match x {
        |    case Neg.MkNeg(_) => 0
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStrictlyPositiveType](result)
  }

  test("NonStrictlyPositiveType.03") {
    // Nested negative position: recursive type inside (T -> X) -> Y
    // The inner (Tricky -> Int32) is in negative position of the outer arrow,
    // which makes Tricky appear in positive position of the outer but negative of the inner.
    // Actually (Tricky -> Int32) -> Int32 has Tricky in positive position overall
    // (negative of negative = positive). Let's use a simpler case.
    val input =
      """
        |enum Tricky { case MkTricky(Tricky -> Int32) }
        |@Terminates
        |def f(x: Tricky): Int32 = match x {
        |    case Tricky.MkTricky(_) => 0
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStrictlyPositiveType](result)
  }

  // =========================================================================
  // ForbiddenExpression
  // =========================================================================

  // --- Unsafe ---
  test("ForbiddenExpression.Unsafe.01") {
    val input =
      """
        |@Terminates
        |def f(): Int32 = unsafe 42
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  // --- Spawn ---
  test("ForbiddenExpression.Spawn.01") {
    val input =
      """
        |@Terminates
        |def f(): Unit \ IO = region rc {
        |    spawn (() -> ()) @ rc
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  // --- Closure application ---
  test("ForbiddenExpression.ClosureApp.01") {
    val input =
      """
        |@Terminates
        |def f(g: Int32 -> Int32, x: Int32): Int32 = g(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  // =========================================================================
  // Trait default implementations
  // =========================================================================

  test("NonStructuralRecursion.Trait.01") {
    // Trait default impl with non-structural self-recursion
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |trait Foo[a] {
        |    @Terminates
        |    pub def foo(x: MyList[a]): Int32 = Foo.foo(x)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("ForbiddenExpression.Trait.01") {
    // Trait default impl with forbidden unsafe expression
    val input =
      """
        |trait Bar[a] {
        |    @Terminates
        |    pub def bar(x: a): Int32 = unsafe 42
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  test("NonStrictlyPositiveType.Trait.01") {
    // Trait default impl with non-strictly positive type
    val input =
      """
        |enum Bad { case MkBad(Bad -> Int32) }
        |trait Baz[a] {
        |    @Terminates
        |    pub def baz(x: Bad): Int32 = match x {
        |        case Bad.MkBad(_) => 0
        |    }
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStrictlyPositiveType](result)
  }

  // =========================================================================
  // NonStructuralRecursion — Local Defs
  // =========================================================================

  test("NonStructuralRecursion.LocalDef.01") {
    // Local def with infinite recursion: loop(ll, acc) = loop(ll, acc + 1)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |pub def evil(l: MyList[Int32]): Int32 =
        |    def loop(ll: MyList[Int32], acc: Int32): Int32 = loop(ll, acc + 1);
        |    loop(l, 0)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.LocalDef.02") {
    // Local def passes alias, not strict sub: let y = ll; loop(y)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |pub def bad(l: MyList[Int32]): Int32 =
        |    def loop(ll: MyList[Int32]): Int32 = match ll {
        |        case MyList.Nil         => 0
        |        case MyList.Cons(_, xs) =>
        |            let y = ll;
        |            loop(y)
        |    };
        |    loop(l)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.LocalDef.03") {
    // Local def calls itself with unchanged arg: helper(ll) = helper(ll)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |pub def bad2(l: MyList[Int32]): Int32 =
        |    def helper(ll: MyList[Int32]): Int32 = helper(ll);
        |    helper(l)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("ForbiddenExpression.LocalDef.01") {
    // Forbidden `unsafe` inside local def body in @Terminates function
    val input =
      """
        |@Terminates
        |pub def badUnsafe(): Int32 =
        |    def helper(): Int32 = unsafe 42;
        |    helper()
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  // =========================================================================
  // Instance implementations
  // =========================================================================

  test("NonStructuralRecursion.Instance.01") {
    // Instance def with non-structural self-recursion
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |trait Foo[a] {
        |    pub def foo(x: MyList[a]): Int32
        |}
        |instance Foo[Int32] {
        |    @Terminates
        |    pub def foo(x: MyList[Int32]): Int32 = Foo.foo(x)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("ForbiddenExpression.Instance.01") {
    // Instance def with forbidden unsafe expression
    val input =
      """
        |trait Bar[a] {
        |    pub def bar(x: a): Int32
        |}
        |instance Bar[Int32] {
        |    @Terminates
        |    def bar(x: Int32): Int32 = unsafe 42
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  // =========================================================================
  // Type Aliases
  // =========================================================================

  test("NonStructuralRecursion.TypeAlias.01") {
    // Type alias for MyList; non-structural recursive call f(x)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |type alias ML = MyList[Int32]
        |@Terminates
        |def f(x: ML): Int32 = f(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStrictlyPositiveType.TypeAlias.01") {
    // Non-strictly-positive enum behind a type alias
    val input =
      """
        |enum Bad { case MkBad(Bad -> Int32) }
        |type alias BadAlias = Bad
        |@Terminates
        |def f(x: BadAlias): Int32 = match x {
        |    case Bad.MkBad(_) => 0
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStrictlyPositiveType](result)
  }

  test("NonStructuralRecursion.TypeAlias.02") {
    // Parameterized type alias; recursive call passes original param instead of substructure
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |type alias ML[a] = MyList[a]
        |@Terminates
        |def f(x: ML[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, xs) => f(x)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

}
