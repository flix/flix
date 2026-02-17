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

}
