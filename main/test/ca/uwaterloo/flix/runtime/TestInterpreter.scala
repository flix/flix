package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.TypedAst.{Expression, Literal, Type}
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

}
