package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.TypedAst.{Expression, Literal, Type}
import ca.uwaterloo.flix.lang.ast.{BinaryOperator, UnaryOperator}
import org.scalatest.FunSuite

class TestInterpreter extends FunSuite {

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Literals                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Unit") {
    val exp = Expression.Lit(Literal.Unit, Type.Unit)
    val result = Interpreter.eval(exp)
    assertResult(Value.Unit)(result)
  }

  test("Bool") {
    val exp01 = Expression.Lit(Literal.Bool(true), Type.Bool)
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(true))(result01)

    val exp02 = Expression.Lit(Literal.Bool(false), Type.Bool)
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(false))(result02)
  }

  test("Int") {
    val exp01 = Expression.Lit(Literal.Int(-242), Type.Int)
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(-242))(result01)

    val exp02 = Expression.Lit(Literal.Int(-42), Type.Int)
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(-42))(result02)

    val exp03 = Expression.Lit(Literal.Int(0), Type.Int)
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(0))(result03)

    val exp04 = Expression.Lit(Literal.Int(98), Type.Int)
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(98))(result04)

    val exp05 = Expression.Lit(Literal.Int(91238), Type.Int)
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(91238))(result05)
  }

  test("Str") {
    val exp01 = Expression.Lit(Literal.Str(""), Type.Str)
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Str(""))(result01)

    val exp02 = Expression.Lit(Literal.Str("Hello World!"), Type.Str)
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Str("Hello World!"))(result02)

    val exp03 = Expression.Lit(Literal.Str("asdf"), Type.Str)
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Str("asdf"))(result03)

    val exp04 = Expression.Lit(Literal.Str("foobar"), Type.Str)
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Str("foobar"))(result04)

    val exp05 = Expression.Lit(Literal.Str("\"\"\""), Type.Str)
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Str("\"\"\""))(result05)
  }

  // TODO(mhyee): Tag

  test("Tuple") {
    val exp01 = Expression.Lit(
      Literal.Tuple(List(Literal.Int(42), Literal.Bool(false), Literal.Str("hi")),
        Type.Tuple(List(Type.Int, Type.Bool, Type.Str))),
      Type.Tuple(List(Type.Int, Type.Bool, Type.Str)))
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Tuple(List(Value.Int(42), Value.Bool(false), Value.Str("hi"))))(result01)

    val exp02 = Expression.Lit(
      Literal.Tuple(List(
        Literal.Int(4),
        Literal.Tuple(List(Literal.Int(12), Literal.Int(8)),
          Type.Tuple(List(Type.Int, Type.Int)))),
        Type.Tuple(List(Type.Int, Type.Tuple(List(Type.Int, Type.Int))))),
      Type.Tuple(List(Type.Int, Type.Tuple(List(Type.Int, Type.Int)))))
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Tuple(List(Value.Int(4), Value.Tuple(List(Value.Int(12), Value.Int(8))))))(result02)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Unary and Binary                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("unary not") {
    val exp01 = Expression.Unary(
      UnaryOperator.Not,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Type.Bool)
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(false))(result01)

    val exp02 = Expression.Unary(
      UnaryOperator.Not,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool)
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(true))(result02)
  }

  test("unary plus") {
    val exp01 = Expression.Unary(
      UnaryOperator.UnaryPlus,
      Expression.Lit(Literal.Int(23), Type.Int),
      Type.Int)
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(23))(result01)

    val exp02 = Expression.Unary(
      UnaryOperator.UnaryPlus,
      Expression.Lit(Literal.Int(-4), Type.Int),
      Type.Int)
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(-4))(result02)
  }

  test("unary minus") {
    val exp01 = Expression.Unary(
      UnaryOperator.UnaryMinus,
      Expression.Lit(Literal.Int(23), Type.Int),
      Type.Int)
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(-23))(result01)

    val exp02 = Expression.Unary(
      UnaryOperator.UnaryMinus,
      Expression.Lit(Literal.Int(-4), Type.Int),
      Type.Int)
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(4))(result02)
  }

  test("binary plus") {
    val exp01 = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(500))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(500))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(-300))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(-100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(300))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(-100), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(-500))(result05)
  }

  test("binary minus") {
    val exp01 = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(300))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(-300))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(-500))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(-100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(-500))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(-100), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(-300))(result05)
  }

  test("binary times") {
    val exp01 = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(2), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(6))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(2), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(6))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(-2), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(-6))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(2), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(-6))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(-2), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(6))(result05)
  }

  test("binary divide") {
    val exp01 = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(4))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(0))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(-4))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(0))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(4))(result05)
  }

  // TODO(mhyee): We need to document the exact semantics of modulo on negative operands
  test("binary modulo") {
    val exp01 = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(2), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(0))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(5), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(2))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(5), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(-2))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(-5), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(2))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-5), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(-2))(result05)
  }

  test("binary less than") {
    val exp01 = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(false))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(true))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(false))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Bool(true))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Bool(false))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Bool(false))(result06)
  }

  test("binary less than or equal") {
    val exp01 = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(false))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(true))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(true))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Bool(true))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Bool(false))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Bool(true))(result06)
  }

  test("binary greater than") {
    val exp01 = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(true))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(false))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(false))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Bool(false))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Bool(true))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Bool(false))(result06)
  }

  test("binary greater than or equal") {
    val exp01 = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(true))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(false))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(true))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Bool(false))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Bool(true))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Bool(true))(result06)
  }

  test("binary equal") {
    val exp01 = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(false))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(false))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(true))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Bool(false))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Bool(false))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Bool(true))(result06)
  }

  test("binary not equal") {
    val exp01 = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(true))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(true))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(false))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Bool(true))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Bool(true))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Bool(false))(result06)
  }

  test("binary and") {
    val exp01 = Expression.Binary(
      BinaryOperator.And,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(true))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.And,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(false))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.And,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(false))(result03)
  }

  test("binary or") {
    val exp01 = Expression.Binary(
      BinaryOperator.Or,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Type.Bool
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Bool(true))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Or,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Bool(true))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Or,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Bool(false))(result03)
  }

  test("binary minimum") {
    val exp01 = Expression.Binary(
     BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(3))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(3))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(3))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(-12))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(-12))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Int(-3))(result06)
  }

  test("binary maximum") {
    val exp01 = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Int(12))(result01)

    val exp02 = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Int(12))(result02)

    val exp03 = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Int(3))(result03)

    val exp04 = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result04 = Interpreter.eval(exp04)
    assertResult(Value.Int(-3))(result04)

    val exp05 = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Int
    )
    val result05 = Interpreter.eval(exp05)
    assertResult(Value.Int(-3))(result05)

    val exp06 = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result06 = Interpreter.eval(exp06)
    assertResult(Value.Int(-3))(result06)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - If Then Else                                              //
  /////////////////////////////////////////////////////////////////////////////

  test("if then else") {
    val exp01 = Expression.IfThenElse(
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Str("foo"), Type.Str),
      Expression.Lit(Literal.Str("bar"), Type.Str),
      Type.Str
    )
    val result01 = Interpreter.eval(exp01)
    assertResult(Value.Str("foo"))(result01)

    val exp02 = Expression.IfThenElse(
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Str("foo"), Type.Str),
      Expression.Lit(Literal.Str("bar"), Type.Str),
      Type.Str
    )
    val result02 = Interpreter.eval(exp02)
    assertResult(Value.Str("bar"))(result02)

    // if (20 % 7 >= 3 || 25 - 5 == 4) "foo" else "bar"
    val exp03 = Expression.IfThenElse(
      Expression.Binary(
        BinaryOperator.Or,
        Expression.Binary(
          BinaryOperator.GreaterEqual,
          Expression.Binary(
            BinaryOperator.Modulo,
            Expression.Lit(Literal.Int(20), Type.Int),
            Expression.Lit(Literal.Int(7), Type.Int),
            Type.Int
          ),
          Expression.Lit(Literal.Int(3), Type.Int),
          Type.Bool
        ),
        Expression.Binary(
          BinaryOperator.Equal,
          Expression.Binary(
            BinaryOperator.Minus,
            Expression.Lit(Literal.Int(25), Type.Int),
            Expression.Lit(Literal.Int(5), Type.Int),
            Type.Int
          ),
          Expression.Lit(Literal.Int(4), Type.Int),
          Type.Bool
        ),
        Type.Bool
      ),
      Expression.Lit(Literal.Str("foo"), Type.Str),
      Expression.Lit(Literal.Str("bar"), Type.Str),
      Type.Str
    )
    val result03 = Interpreter.eval(exp03)
    assertResult(Value.Str("foo"))(result03)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Tuples and Tags                                           //
  /////////////////////////////////////////////////////////////////////////////

  // Note: These are tuple and tag *expressions*, not *literals* (which are tested above)
  // TODO(mhyee)
}
