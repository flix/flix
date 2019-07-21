package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.errors.TrivialError
import ca.uwaterloo.flix.util.Options
import org.scalatest.FunSuite

class TestTrivial extends FunSuite with TestUtils {

  val DefaultOptions: Options = Options.DefaultTest.copy(core = true)

  ignore("TrivialExpression.LeftAdditionByZero") {
    val input =
      """
        |pub def f(): Int = 0 + 123
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.RightAdditionByZero") {
    val input =
      """
        |pub def f(): Int = 123 + 0
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.SubtractionByZero") {
    val input =
      """
        |pub def f(): Int = 123 - 0
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.SubtractionBySelf") {
    val input =
      """
        |pub def f(x: Int): Int = x - x
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.LeftMultiplicationByZero") {
    val input =
      """
        |pub def f(): Int = 0 * 123
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.RightMultiplicationByZero") {
    val input =
      """
        |pub def f(): Int = 123 * 0
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.LeftMultiplicationByOne") {
    val input =
      """
        |pub def f(): Int = 1 * 123
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.RightMultiplicationByOne") {
    val input =
      """
        |pub def f(): Int = 123 * 1
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.DivisionByOne") {
    val input =
      """
        |pub def f(): Int = 123 / 1
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.DivisionBySelf") {
    val input =
      """
        |pub def f(x: Int): Int = x / x
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.LeftConcatenateEmptyString") {
    val input =
      """
        |pub def f(): Str = "" + "Hello World"
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.RightConcatenateEmptyString") {
    val input =
      """
        |pub def f(): Str = "Hello World" + ""
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.LeftAppendNil") {
    val input =
      """
        |pub def f(): List[Int] = Nil ::: (1 :: 2 :: Nil)
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |namespace List {
        |    pub def append[a](xs: List[a], ys: List[a]): List[a] = match xs with {
        |        case Nil => ys
        |        case x :: rs => x :: append(rs, ys)
        |    }
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.RightAppendNil") {
    val input =
      """
        |pub def f(): List[Int] = (1 :: 2 :: Nil) ::: Nil
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |namespace List {
        |    pub def append[a](xs: List[a], ys: List[a]): List[a] = match xs with {
        |        case Nil => ys
        |        case x :: rs => x :: append(rs, ys)
        |    }
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.ListIsEmptyCons") {
    val input =
      """
        |pub def f(): Bool = List.isEmpty(1 :: Nil)
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |namespace List {
        |    pub def isEmpty[a](xs: List[a]): Bool = match xs with {
        |        case Nil => true
        |        case _ => false
        |    }
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

  ignore("TrivialExpression.ListMapIdentity") {
    val input =
      """
        |pub def f(): List[Int] = List.map(x -> x, 1 :: 2 :: Nil)
        |
        |enum List[t] {
        |    case Nil,
        |    case Cons(t, List[t])
        |}
        |
        |namespace List {
        |    pub def map[a,b](f: a -> b, xs: List[a]): List[b] = match xs with {
        |        case Nil => Nil
        |        case x :: rs => f(x) :: map(f, rs)
        |    }
        |}
        |
      """.stripMargin
    val result = compile(input, DefaultOptions)
    expectError[TrivialError.TrivialExpression](result)
  }

}
