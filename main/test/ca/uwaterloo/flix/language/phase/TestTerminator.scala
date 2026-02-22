package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TerminationError.{ForbiddenExpression, NonRecursiveLocalTailRec, NonRecursiveTailRec, NonStrictlyPositiveType, NonStructuralRecursion, NonTailRecursiveCall, NonTailRecursiveLocalCall, NonTerminatingCall}
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

  test("NonStructuralRecursion.08") {
    // Recursive call with a non-variable expression (ArgStatus.NotAVariable)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, xs) => f(MyList.Nil)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.09") {
    // Strict sub of wrong param: xs is sub of x, passed in y's position; y passed in x's position
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32], y: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, xs) => f(y, xs)
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

  test("NonStrictlyPositiveType.04") {
    // Non-strictly-positive type on a local def parameter
    val input =
      """
        |enum Bad { case MkBad(Bad -> Int32) }
        |@Terminates
        |def f(x: Int32): Int32 =
        |    def loop(b: Bad): Int32 = match b {
        |        case Bad.MkBad(_) => 0
        |    };
        |    loop(Bad.MkBad(_ -> 0))
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStrictlyPositiveType](result)
  }

  test("NonStrictlyPositiveType.Instance.01") {
    // Non-strictly-positive type on an instance def parameter
    val input =
      """
        |enum Bad { case MkBad(Bad -> Int32) }
        |trait Baz[a] {
        |    pub def baz(x: a): Int32
        |}
        |instance Baz[Bad] {
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
    // Locally-constructed closure application is forbidden
    val input =
      """
        |@Terminates
        |def f(x: Int32): Int32 =
        |    let c = y -> y + 1;
        |    c(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  test("ForbiddenExpression.ClosureApp.02") {
    // Closure from local def param (not top-level param) is forbidden
    val input =
      """
        |@Terminates
        |def f(x: Int32): Int32 =
        |    def loop(g: Int32 -> Int32): Int32 = g(x);
        |    loop(y -> y + 1)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  test("ForbiddenExpression.ClosureApp.03") {
    // Applying a non-variable expression as closure is forbidden
    val input =
      """
        |@Terminates
        |def f(x: Int32): Int32 = (y -> y + 1)(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  test("ForbiddenExpression.ClosureApp.04") {
    // Let-bound to a non-param value, then aliased — forbidden
    val input =
      """
        |@Terminates
        |def f(x: Int32): Int32 =
        |    let g = (y: Int32) -> y + 1;
        |    let h = g;
        |    h(x)
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

  test("NonStructuralRecursion.LocalDef.OuterCall.01") {
    // Local def calls outer function with its own param (untracked in outer env)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 =
        |    def loop(ll: MyList[Int32]): Int32 = f(ll);
        |    loop(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("NonStructuralRecursion.LocalDef.OuterCall.02") {
    // Deeply nested local def calls outermost function non-structurally
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(x: MyList[Int32]): Int32 =
        |    def g(y: MyList[Int32]): Int32 =
        |        def h(z: MyList[Int32]): Int32 = f(z);
        |        h(y);
        |    g(x)
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
  // NonTerminatingCall
  // =========================================================================

  test("NonTerminatingCall.01") {
    // Calling a non-@Terminates def from a @Terminates function
    val input =
      """
        |def g(x: Int32): Int32 = x + 1
        |@Terminates
        |def f(x: Int32): Int32 = g(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTerminatingCall](result)
  }

  test("NonTerminatingCall.02") {
    // Calling a non-@Terminates def inside a match branch
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |def helper(x: Int32): Int32 = x * 2
        |@Terminates
        |def f(l: MyList[Int32]): Int32 = match l {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(x, xs) => helper(x) + f(xs)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTerminatingCall](result)
  }

  test("NonTerminatingCall.03") {
    // Multiple non-@Terminates calls
    val input =
      """
        |def g(x: Int32): Int32 = x
        |def h(x: Int32): Int32 = x
        |@Terminates
        |def f(x: Int32): Int32 = g(x) + h(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTerminatingCall](result)
  }

  test("NonTerminatingCall.04") {
    // Non-@Terminates call inside a local def body
    val input =
      """
        |def helper(x: Int32): Int32 = x + 1
        |@Terminates
        |def f(x: Int32): Int32 =
        |    def loop(y: Int32): Int32 = helper(y);
        |    loop(x)
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTerminatingCall](result)
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

  // =========================================================================
  // NonTailRecursiveCall
  // =========================================================================

  test("NonTailRecursiveCall.01") {
    // Self-call used as operand in addition: f(xs) + 1
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Tailrec
        |def f(x: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, xs) => f(xs) + 1
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveCall](result)
  }

  test("NonTailRecursiveCall.02") {
    // Self-call as argument to another function
    val input =
      """
        |@Terminates
        |def g(x: Int32): Int32 = x
        |@Tailrec
        |def f(x: Int32): Int32 = g(f(x))
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveCall](result)
  }

  test("NonTailRecursiveCall.03") {
    // Self-call in let binding (not tail)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Tailrec
        |def f(x: MyList[Int32]): Int32 = match x {
        |    case MyList.Nil         => 0
        |    case MyList.Cons(_, xs) =>
        |        let r = f(xs);
        |        r
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveCall](result)
  }

  test("NonTailRecursiveCall.04") {
    // Self-call in both branches of if, but wrapped in Cons (not tail)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Tailrec
        |def f(x: MyList[Int32]): MyList[Int32] = match x {
        |    case MyList.Nil         => MyList.Nil
        |    case MyList.Cons(_, xs) => MyList.Cons(0, f(xs))
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveCall](result)
  }

  test("NonTailRecursiveCall.05") {
    // Two self-calls: left one is non-tail (operand of +)
    val input =
      """
        |enum MyTree[a] { case Leaf(a), case Node(MyTree[a], MyTree[a]) }
        |@Tailrec
        |def f(t: MyTree[Int32]): Int32 = match t {
        |    case MyTree.Leaf(v)    => v
        |    case MyTree.Node(l, r) => f(l) + f(r)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveCall](result)
  }

  // =========================================================================
  // NonTailRecursiveLocalCall
  // =========================================================================

  test("NonTailRecursiveLocalCall.01") {
    // @Tailrec local def with non-tail self-call (operand of +)
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |def f(l: MyList[Int32]): Int32 = {
        |    @Tailrec
        |    def loop(ll: MyList[Int32]): Int32 = match ll {
        |        case MyList.Nil         => 0
        |        case MyList.Cons(_, xs) => loop(xs) + 1
        |    };
        |    loop(l)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveLocalCall](result)
  }

  test("NonTailRecursiveLocalCall.02") {
    // @Tailrec local def with self-call in let binding
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |def f(l: MyList[Int32]): Int32 = {
        |    @Tailrec
        |    def loop(ll: MyList[Int32]): Int32 = match ll {
        |        case MyList.Nil         => 0
        |        case MyList.Cons(_, xs) =>
        |            let r = loop(xs);
        |            r
        |    };
        |    loop(l)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveLocalCall](result)
  }

  test("NonTailRecursiveLocalCall.03") {
    // @Tailrec @Terminates local def with non-tail self-call
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Terminates
        |def f(l: MyList[Int32]): Int32 = {
        |    @Tailrec @Terminates
        |    def loop(ll: MyList[Int32]): Int32 = match ll {
        |        case MyList.Nil         => 0
        |        case MyList.Cons(_, xs) => loop(xs) + 1
        |    };
        |    loop(l)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonTailRecursiveLocalCall](result)
  }

  // =========================================================================
  // Standalone @Terminates local defs — negative tests
  // =========================================================================

  test("NonStructuralRecursion.StandaloneLocalDef.01") {
    // Standalone @Terminates local def with non-structural recursion
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |def f(l: MyList[Int32]): Int32 = {
        |    @Terminates
        |    def loop(ll: MyList[Int32]): Int32 = loop(ll);
        |    loop(l)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonStructuralRecursion](result)
  }

  test("ForbiddenExpression.StandaloneLocalDef.01") {
    // Standalone @Terminates local def with unsafe
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |def f(l: MyList[Int32]): Int32 = {
        |    @Terminates
        |    def loop(ll: MyList[Int32]): Int32 = match ll {
        |        case MyList.Nil         => 0
        |        case MyList.Cons(_, xs) => unsafe { loop(xs) + 1 }
        |    };
        |    loop(l)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[ForbiddenExpression](result)
  }

  // =========================================================================
  // NonRecursiveTailRec
  // =========================================================================

  test("NonRecursiveTailRec.01") {
    // @Tailrec on a non-recursive function (no self-calls at all)
    val input =
      """
        |@Tailrec
        |def f(x: Int32): Int32 = x + 1
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonRecursiveTailRec](result)
  }

  test("NonRecursiveTailRec.02") {
    // @Tailrec on a function that only delegates to an inner loop
    val input =
      """
        |enum MyList[a] { case Nil, case Cons(a, MyList[a]) }
        |@Tailrec
        |def f(l: MyList[Int32]): Int32 = {
        |    def loop(ll: MyList[Int32], acc: Int32): Int32 = match ll {
        |        case MyList.Nil         => acc
        |        case MyList.Cons(_, xs) => loop(xs, acc + 1)
        |    };
        |    loop(l, 0)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonRecursiveTailRec](result)
  }

  // =========================================================================
  // NonRecursiveLocalTailRec
  // =========================================================================

  test("NonRecursiveLocalTailRec.01") {
    // @Tailrec on a non-recursive local def (no self-calls)
    val input =
      """
        |def f(x: Int32): Int32 = {
        |    @Tailrec
        |    def loop(y: Int32): Int32 = y + 1;
        |    loop(x)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonRecursiveLocalTailRec](result)
  }

  test("NonRecursiveLocalTailRec.02") {
    // @Tailrec on a local def that calls the outer function, not itself
    val input =
      """
        |def f(x: Int32): Int32 = {
        |    @Tailrec
        |    def loop(y: Int32): Int32 = f(y);
        |    loop(x)
        |}
      """.stripMargin
    val result = check(input, Options.TestWithLibNix)
    expectError[NonRecursiveLocalTailRec](result)
  }

}
