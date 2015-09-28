package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.lang.ast.TypedAst.{Expression, Literal, Type, FormalArg, Root}
import ca.uwaterloo.flix.lang.ast.{BinaryOperator, UnaryOperator, ParsedAst, Name, SourceLocation}
import org.scalatest.FunSuite

class TestInterpreter extends FunSuite {
  val root = Root(Map(), Map(), Map(), List(), List())

  object ConstantPropTagDefs {
    val name = Name.Resolved(List("ConstProp"))
    val identB = ParsedAst.Ident("Bot", SourceLocation(None, 0, 0))
    val identV = ParsedAst.Ident("Val", SourceLocation(None, 0, 0))
    val identT = ParsedAst.Ident("Top", SourceLocation(None, 0, 0))

    val tagTpeB = Type.Tag(name, identB, Type.Unit)
    val tagTpeV = Type.Tag(name, identV, Type.Int)
    val tagTpeT = Type.Tag(name, identT, Type.Unit)
    val enumTpe = Type.Enum(Map("ConstProp.Bot" -> tagTpeB, "ConstProp.Val" -> tagTpeV, "ConstProp.Top" -> tagTpeT))
  }

  val ident01 = ParsedAst.Ident("x", SourceLocation(None, 0, 0))
  val ident02 = ParsedAst.Ident("y", SourceLocation(None, 0, 0))
  val ident03 = ParsedAst.Ident("z", SourceLocation(None, 0, 0))

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Literals                                                  //
  /////////////////////////////////////////////////////////////////////////////

  test("Literal.Unit") {
    val input = Expression.Lit(Literal.Unit, Type.Unit)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Unit)(result)
  }

  test("Literal.Bool01") {
    val input = Expression.Lit(Literal.Bool(true), Type.Bool)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("Literal.Bool02") {
    val input = Expression.Lit(Literal.Bool(false), Type.Bool)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("Literal.Int01") {
    val input = Expression.Lit(Literal.Int(-242), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-242))(result)
  }

  test("Literal.Int02") {
    val input = Expression.Lit(Literal.Int(-42), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-42))(result)
  }

  test("Literal.Int03") {
    val input = Expression.Lit(Literal.Int(0), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(0))(result)
  }

  test("Literal.Int04") {
    val input = Expression.Lit(Literal.Int(98), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(98))(result)
  }

  test("Literal.Int05") {
    val input = Expression.Lit(Literal.Int(91238), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(91238))(result)
  }

  test("Literal.Str01") {
    val input = Expression.Lit(Literal.Str(""), Type.Str)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str(""))(result)
  }

  test("Literal.Str02") {
    val input = Expression.Lit(Literal.Str("Hello World!"), Type.Str)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("Hello World!"))(result)
  }

  test("Literal.Str03") {
    val input = Expression.Lit(Literal.Str("asdf"), Type.Str)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("asdf"))(result)
  }

  test("Literal.Str04") {
    val input = Expression.Lit(Literal.Str("foobar"), Type.Str)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("foobar"))(result)
  }

  test("Literal.Str05") {
    val input = Expression.Lit(Literal.Str("\"\"\""), Type.Str)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("\"\"\""))(result)
  }

  test("Literal.Tuple01") {
    val input = Expression.Lit(
      Literal.Tuple(List(Literal.Int(42), Literal.Bool(false), Literal.Str("hi")),
        Type.Tuple(List(Type.Int, Type.Bool, Type.Str))),
      Type.Tuple(List(Type.Int, Type.Bool, Type.Str)))
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tuple(List(Value.Int(42), Value.Bool(false), Value.Str("hi"))))(result)
  }

  test("Literal.Tuple02") {
    val input = Expression.Lit(
      Literal.Tuple(List(
        Literal.Int(4),
        Literal.Tuple(List(Literal.Int(12), Literal.Int(8)),
          Type.Tuple(List(Type.Int, Type.Int)))),
        Type.Tuple(List(Type.Int, Type.Tuple(List(Type.Int, Type.Int))))),
      Type.Tuple(List(Type.Int, Type.Tuple(List(Type.Int, Type.Int)))))
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tuple(List(Value.Int(4), Value.Tuple(List(Value.Int(12), Value.Int(8))))))(result)
  }

  test("Literal.Tag01") {
    val name = Name.Resolved(List("foo", "bar"))
    val ident = ParsedAst.Ident("baz", SourceLocation(None, 0, 0))
    val tagTpe = Type.Tag(name, ident, Type.Str)
    val enumTpe = Type.Enum(Map("foo.bar.baz" -> tagTpe))
    val input = Expression.Lit(Literal.Tag(name, ident, Literal.Str("hello world"), enumTpe), tagTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "baz", Value.Str("hello world")))(result)
  }

  test("Literal.Tag02") {
    val name = Name.Resolved(List("Family"))
    val ident = ParsedAst.Ident("NameAndAge", SourceLocation(None, 0, 0))
    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int)))
    val enumTpe = Type.Enum(Map("Family.NameAndAge" -> tagTpe))
    val input = Expression.Lit(Literal.Tag(name, ident, Literal.Tuple(List(Literal.Str("James"), Literal.Int(42)),
      Type.Tuple(List(Type.Str, Type.Int))), enumTpe), tagTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "NameAndAge", Value.Tuple(List(Value.Str("James"), Value.Int(42)))))(result)
  }

  test("Literal.Tag03a") {
    import ConstantPropTagDefs._
    val input = Expression.Lit(Literal.Tag(name, identB, Literal.Unit, enumTpe), tagTpeB)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Bot", Value.Unit))(result)
  }

  test("Literal.Tag03b") {
    import ConstantPropTagDefs._
    val input = Expression.Lit(Literal.Tag(name, identT, Literal.Unit, enumTpe), tagTpeT)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Top", Value.Unit))(result)
  }

  test("Literal.Tag03c") {
    import ConstantPropTagDefs._
    val input = Expression.Lit(Literal.Tag(name, identV, Literal.Int(0), enumTpe), tagTpeV)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Val", Value.Int(0)))(result)

  }

  test("Literal.Tag03d") {
    import ConstantPropTagDefs._
    val input = Expression.Lit(Literal.Tag(name, identV, Literal.Int(-240), enumTpe), tagTpeV)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Val", Value.Int(-240)))(result)
  }

  test("Literal.Tag03e") {
    import ConstantPropTagDefs._
    val input = Expression.Lit(Literal.Tag(name, identV, Literal.Int(1241), enumTpe), tagTpeV)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Val", Value.Int(1241)))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Var                                                       //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Var01") {
    val input = Expression.Lit(Literal.Str("hello"), Type.Str)
    val env = Map(ident01 -> Value.Bool(false))
    val result = Interpreter.eval(input, root, env)
    assertResult(Value.Str("hello"))(result)
  }

  test("Expression.Var02") {
    val input = Expression.Var(ident01, Type.Int)
    val env = Map(ident01 -> Value.Int(5))
    val result = Interpreter.eval(input, root, env)
    assertResult(Value.Int(5))(result)
  }

  test("Expression.Var03") {
    val input = Expression.Var(ident01, Type.Bool)
    val env = Map(ident01 -> Value.Bool(false))
    val result = Interpreter.eval(input, root, env)
    assertResult(Value.Bool(false))(result)
  }

  test("Expression.Var04") {
    val input = Expression.Var(ident02, Type.Str)
    val env = Map(ident01 -> Value.Str("foo"), ident02 -> Value.Str("bar"))
    val result = Interpreter.eval(input, root, env)
    assertResult(Value.Str("bar"))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Lambda and Apply                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Lambda01") {
    // () => false
    val lambda = Expression.Lambda(
      List(),
      Type.Bool,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Function(List(), Type.Bool))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (() => false)()
    val apply = Expression.Apply(lambda, List(), Type.Bool)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Bool(false))(value)
  }

  test("Expression.Lambda02") {
    // x => 3
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Int)),
      Type.Int,
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Function(List(Type.Int), Type.Int))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (x => 3)(4)
    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(4), Type.Int)), Type.Int)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Int(3))(value)
  }

  test("Expression.Lambda03") {
    // x => x
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Int)),
      Type.Int,
      Expression.Var(ident01, Type.Int),
      Type.Function(List(Type.Int), Type.Int))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (x => x)(5)
    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(5), Type.Int)), Type.Int)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Int(5))(value)
  }

  test("Expression.Lambda04") {
    // x => 1 + 2
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Int)),
      Type.Int,
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Lit(Literal.Int(1), Type.Int),
        Expression.Lit(Literal.Int(2), Type.Int),
        Type.Int),
      Type.Function(List(Type.Int), Type.Int))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (x => 1 + 2)(42)
    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(42), Type.Int)), Type.Int)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Int(3))(value)
  }

  test("Expression.Lambda05") {
    // x => x + 2
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Int)),
      Type.Int,
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Var(ident01, Type.Int),
        Expression.Lit(Literal.Int(2), Type.Int),
        Type.Int),
      Type.Function(List(Type.Int), Type.Int))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (x => x + 2)(100)
    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(100), Type.Int)), Type.Int)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Int(102))(value)
  }

  test("Expression.Lambda06") {
    // (x, y) => x + y
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Int), FormalArg(ident02, Type.Int)),
      Type.Int,
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Var(ident01, Type.Int),
        Expression.Var(ident02, Type.Int),
        Type.Int),
      Type.Function(List(Type.Int, Type.Int), Type.Int))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (x, y => x + y)(3, 4)
    val apply = Expression.Apply(lambda, List(
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(4), Type.Int)),
      Type.Int)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Int(7))(value)
  }

  test("Expression.Lambda07") {
    // (x, y) => if (x) then true else y
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Bool), FormalArg(ident02, Type.Bool)),
      Type.Int,
      Expression.IfThenElse(
        Expression.Var(ident01, Type.Bool),
        Expression.Lit(Literal.Bool(true), Type.Bool),
        Expression.Var(ident02, Type.Bool),
        Type.Bool),
      Type.Function(List(Type.Bool, Type.Bool), Type.Bool))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // ((x, y) => if (x) then true else y)(false, true)
    val apply = Expression.Apply(lambda, List(
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Bool(true), Type.Bool)),
      Type.Bool)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Bool(true))(value)
  }

  test("Expression.Lambda08") {
    // (x, y, z) => x + (y + z)
    val lambda = Expression.Lambda(
      List(FormalArg(ident01, Type.Int), FormalArg(ident02, Type.Int), FormalArg(ident03, Type.Int)),
      Type.Int,
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Var(ident01, Type.Int),
        Expression.Binary(
          BinaryOperator.Plus,
          Expression.Var(ident02, Type.Int),
          Expression.Var(ident03, Type.Int),
          Type.Int),
        Type.Int),
      Type.Function(List(Type.Int, Type.Int, Type.Int), Type.Int))
    val expected = Value.Closure(lambda.formals, lambda.body, Map())
    val closure = Interpreter.eval(lambda, root)
    assertResult(expected)(closure)

    // (x, y, z => x + (y + z))(2, 42, 5)
    val apply = Expression.Apply(lambda, List(
      Expression.Lit(Literal.Int(2), Type.Int),
      Expression.Lit(Literal.Int(42), Type.Int),
      Expression.Lit(Literal.Int(5), Type.Int)),
      Type.Int)
    val value = Interpreter.eval(apply, root)
    assertResult(Value.Int(49))(value)
  }

  // TODO(mhyee): Tests for Lambda and Apply. Nested functions? Higher-order functions?

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Unary and Binary                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("UnaryOperator.Not01") {
    val input = Expression.Unary(
      UnaryOperator.Not,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Type.Bool)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("UnaryOperator.Not02") {
    val input = Expression.Unary(
      UnaryOperator.Not,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("UnaryOperator.UnaryPlus01") {
    val input = Expression.Unary(
      UnaryOperator.UnaryPlus,
      Expression.Lit(Literal.Int(23), Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(23))(result)
  }

  test("UnaryOperator.UnaryPlus02") {
    val input = Expression.Unary(
      UnaryOperator.UnaryPlus,
      Expression.Lit(Literal.Int(-4), Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-4))(result)
  }

  test("UnaryOperator.UnaryMinus03") {
    val input = Expression.Unary(
      UnaryOperator.UnaryMinus,
      Expression.Lit(Literal.Int(23), Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-23))(result)
  }

  test("UnaryOperator.UnaryMinus02") {
    val input = Expression.Unary(
      UnaryOperator.UnaryMinus,
      Expression.Lit(Literal.Int(-4), Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(4))(result)
  }

  test("BinaryOperator.Plus01") {
    val input = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(500))(result)
  }

  test("BinaryOperator.Plus02") {
    val input = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(500))(result)
  }

  test("BinaryOperator.Plus03") {
    val input = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-300))(result)
  }

  test("BinaryOperator.Plus04") {
    val input = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(-100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(300))(result)
  }

  test("BinaryOperator.Plus05") {
    val input = Expression.Binary(
      BinaryOperator.Plus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(-100), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-500))(result)
  }

  test("BinaryOperator.Minus01") {
    val input = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(300))(result)
  }

  test("BinaryOperator.Minus02") {
    val input = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-300))(result)
  }

  test("BinaryOperator.Minus03") {
    val input = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(100), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-500))(result)
  }

  test("BinaryOperator.Minus04") {
    val input = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(-100), Type.Int),
      Expression.Lit(Literal.Int(400), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-500))(result)
  }

  test("BinaryOperator.Minus05") {
    val input = Expression.Binary(
      BinaryOperator.Minus,
      Expression.Lit(Literal.Int(-400), Type.Int),
      Expression.Lit(Literal.Int(-100), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-300))(result)
  }

  test("BinaryOperator.Times01") {
    val input = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(2), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(6))(result)
  }

  test("BinaryOperator.Times02") {
    val input = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(2), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(6))(result)
  }

  test("BinaryOperator.Times03") {
    val input = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(-2), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-6))(result)
  }

  test("BinaryOperator.Times04") {
    val input = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(2), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-6))(result)
  }

  test("BinaryOperator.Times05") {
    val input = Expression.Binary(
      BinaryOperator.Times,
      Expression.Lit(Literal.Int(-2), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(6))(result)
  }

  test("BinaryOperator.Divide01") {
    val input = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(4))(result)
  }

  test("BinaryOperator.Divide02") {
    val input = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(0))(result)
  }

  test("BinaryOperator.Divide03") {
    val input = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-4))(result)
  }

  test("BinaryOperator.Divide04") {
    val input = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(0))(result)
  }

  test("BinaryOperator.Divide05") {
    val input = Expression.Binary(
      BinaryOperator.Divide,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(4))(result)
  }

  // TODO(mhyee): We need to document the exact semantics of modulo on negative operands
  test("BinaryOperator.Modulo01") {
    val input = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(2), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(0))(result)
  }

  test("BinaryOperator.Modulo02") {
    val input = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(5), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(2))(result)
  }

  test("BinaryOperator.Modulo03") {
    val input = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(5), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-2))(result)
  }

  test("BinaryOperator.Modulo04") {
    val input = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(-5), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(2))(result)
  }

  test("BinaryOperator.Modulo05") {
    val input = Expression.Binary(
      BinaryOperator.Modulo,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-5), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-2))(result)
  }

  test("BinaryOperator.Less01") {
    val input = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Less02") {
    val input = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Less03") {
    val input = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Less04") {
    val input = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Less05") {
    val input = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Less06") {
    val input = Expression.Binary(
      BinaryOperator.Less,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.LessEqual01") {
    val input = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.LessEqual02") {
    val input = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.LessEqual03") {
    val input = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.LessEqual04") {
    val input = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.LessEqual05") {
    val input = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.LessEqual06") {
    val input = Expression.Binary(
      BinaryOperator.LessEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Greater01") {
    val input = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Greater02") {
    val input = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Greater03") {
    val input = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Greater04") {
    val input = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Greater05") {
    val input = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Greater06") {
    val input = Expression.Binary(
      BinaryOperator.Greater,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.GreaterEqual01") {
    val input = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.GreaterEqual02") {
    val input = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.GreaterEqual03") {
    val input = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.GreaterEqual04") {
    val input = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.GreaterEqual05") {
    val input = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.GreaterEqual06") {
    val input = Expression.Binary(
      BinaryOperator.GreaterEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Equal01") {
    val input = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Equal02") {
    val input = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Equal03") {
    val input = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Equal04") {
    val input = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Equal05") {
    val input = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Equal06") {
    val input = Expression.Binary(
      BinaryOperator.Equal,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.NotEqual01") {
    val input = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.NotEqual02") {
    val input = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.NotEqual03") {
    val input = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.NotEqual04") {
    val input = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.NotEqual05") {
    val input = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.NotEqual06") {
    val input = Expression.Binary(
      BinaryOperator.NotEqual,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.And01") {
    val input = Expression.Binary(
      BinaryOperator.And,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.And02") {
    val input = Expression.Binary(
      BinaryOperator.And,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.And03") {
    val input = Expression.Binary(
      BinaryOperator.And,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Or01") {
    val input = Expression.Binary(
      BinaryOperator.Or,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Or02") {
    val input = Expression.Binary(
      BinaryOperator.Or,
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(true))(result)
  }

  test("BinaryOperator.Or03") {
    val input = Expression.Binary(
      BinaryOperator.Or,
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Type.Bool
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Bool(false))(result)
  }

  test("BinaryOperator.Minimum01") {
    val input = Expression.Binary(
     BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(3))(result)
  }

  test("BinaryOperator.Minimum02") {
    val input = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(3))(result)
  }

  test("BinaryOperator.Minimum03") {
    val input = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(3))(result)
  }

  test("BinaryOperator.Minimum04") {
    val input = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-12))(result)
  }

  test("BinaryOperator.Minimum05") {
    val input = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-12))(result)
  }

  test("BinaryOperator.Minimum06") {
    val input = Expression.Binary(
      BinaryOperator.Minimum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-3))(result)
  }

  test("BinaryOperator.Maximum01") {
    val input = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(12), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(12))(result)
  }

  test("BinaryOperator.Maximum02") {
    val input = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(12), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(12))(result)
  }

  test("BinaryOperator.Maximum03") {
    val input = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(3), Type.Int),
      Expression.Lit(Literal.Int(3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(3))(result)
  }

  test("BinaryOperator.Maximum04") {
    val input = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(-12), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-3))(result)
  }

  test("BinaryOperator.Maximum05") {
    val input = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-12), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-3))(result)
  }

  test("BinaryOperator.Maximum06") {
    val input = Expression.Binary(
      BinaryOperator.Maximum,
      Expression.Lit(Literal.Int(-3), Type.Int),
      Expression.Lit(Literal.Int(-3), Type.Int),
      Type.Int
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(-3))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - If Then Else                                              //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.IfThenElse01") {
    val input = Expression.IfThenElse(
      Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Str("foo"), Type.Str),
      Expression.Lit(Literal.Str("bar"), Type.Str),
      Type.Str
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("foo"))(result)
  }

  test("Expression.IfThenElse02") {
    val input = Expression.IfThenElse(
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Str("foo"), Type.Str),
      Expression.Lit(Literal.Str("bar"), Type.Str),
      Type.Str
    )
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("bar"))(result)
  }

  test("Expression.IfThenElse03") {
    // if (20 % 7 >= 3 || 25 - 5 == 4) "foo" else "bar"
    val input = Expression.IfThenElse(
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
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("foo"))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Let                                                       //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Let01") {
    // let x = true in 42
    val input = Expression.Let(ident01, Expression.Lit(Literal.Bool(true), Type.Bool),
      Expression.Lit(Literal.Int(42), Type.Int), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(42))(result)
  }

  test("Expression.Let02") {
    // let x = 24 in x
    val input = Expression.Let(ident01, Expression.Lit(Literal.Int(24), Type.Int),
      Expression.Var(ident01, Type.Int), Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(24))(result)
  }

  test("Expression.Let03") {
    // let x = 1 in x + 2
    val input = Expression.Let(ident01, Expression.Lit(Literal.Int(1), Type.Int),
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Var(ident01, Type.Int),
        Expression.Lit(Literal.Int(2), Type.Int),
        Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(3))(result)
  }

  test("Expression.Let04") {
    // let x = false in if x then "abc" else "xyz"
    val input = Expression.Let(ident01, Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.IfThenElse(
        Expression.Var(ident01, Type.Bool),
        Expression.Lit(Literal.Str("abc"), Type.Str),
        Expression.Lit(Literal.Str("xyz"), Type.Str),
        Type.Str),
      Type.Str)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Str("xyz"))(result)
  }

  test("Expression.Let05") {
    // let x = 14 - 3 in x + 2
    val input = Expression.Let(ident01,
      Expression.Binary(
        BinaryOperator.Minus,
        Expression.Lit(Literal.Int(14), Type.Int),
        Expression.Lit(Literal.Int(3), Type.Int),
        Type.Int),
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Var(ident01, Type.Int),
        Expression.Lit(Literal.Int(2), Type.Int),
        Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(13))(result)
  }

  test("Expression.Let06") {
    // let x = 14 - 3 in let y = 2 * 4 in x + y
    val input = Expression.Let(ident01,
      Expression.Binary(
        BinaryOperator.Minus,
        Expression.Lit(Literal.Int(14), Type.Int),
        Expression.Lit(Literal.Int(3), Type.Int),
        Type.Int),
      Expression.Let(ident02,
        Expression.Binary(
          BinaryOperator.Times,
          Expression.Lit(Literal.Int(2), Type.Int),
          Expression.Lit(Literal.Int(4), Type.Int),
          Type.Int),
        Expression.Binary(
          BinaryOperator.Plus,
          Expression.Var(ident01, Type.Int),
          Expression.Var(ident02, Type.Int),
          Type.Int),
        Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(19))(result)
  }

  test("Expression.Let07") {
    // let x = 1 in let y = x + 2 in let z = y + 3 in z
    val input = Expression.Let(ident01,
      Expression.Lit(Literal.Int(1), Type.Int),
      Expression.Let(ident02,
        Expression.Binary(
          BinaryOperator.Plus,
          Expression.Var(ident01, Type.Int),
          Expression.Lit(Literal.Int(2), Type.Int),
          Type.Int),
        Expression.Let(ident03,
          Expression.Binary(
            BinaryOperator.Plus,
            Expression.Var(ident02, Type.Int),
            Expression.Lit(Literal.Int(3), Type.Int),
            Type.Int),
          Expression.Var(ident03, Type.Int),
          Type.Int),
        Type.Int),
      Type.Int)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Int(6))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Match                                                     //
  /////////////////////////////////////////////////////////////////////////////

  // TODO(mhyee): Match expressions and unifier

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Tuples and Tags                                           //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Tuple01") {
    val input = Expression.Tuple(List(
      Expression.Lit(Literal.Int(42), Type.Int),
      Expression.Lit(Literal.Bool(false), Type.Bool),
      Expression.Lit(Literal.Str("hi"), Type.Str)),
      Type.Tuple(List(Type.Int, Type.Bool, Type.Str)))
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tuple(List(Value.Int(42), Value.Bool(false), Value.Str("hi"))))(result)
  }

  test("Expression.Tuple02") {
    val input = Expression.Tuple(List(
      Expression.Lit(Literal.Int(4), Type.Int),
      Expression.Tuple(List(Expression.Lit(Literal.Int(12), Type.Int), Expression.Lit(Literal.Int(8), Type.Int)),
        Type.Tuple(List(Type.Int, Type.Int)))),
      Type.Tuple(List(Type.Int, Type.Tuple(List(Type.Int, Type.Int)))))
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tuple(List(Value.Int(4), Value.Tuple(List(Value.Int(12), Value.Int(8))))))(result)
  }

  test("Expression.Tuple03") {
    val input = Expression.Tuple(List(
      // 40 + 2
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Lit(Literal.Int(40), Type.Int),
        Expression.Lit(Literal.Int(2), Type.Int),
        Type.Int),
      // !(-12 < 22)
      Expression.Unary(
        UnaryOperator.Not,
        Expression.Binary(
          BinaryOperator.Less,
          Expression.Lit(Literal.Int(-12), Type.Int),
          Expression.Lit(Literal.Int(22), Type.Int),
          Type.Bool),
        Type.Bool),
      // if (true) "hi" else "hello"
      Expression.IfThenElse(
        Expression.Lit(Literal.Bool(true), Type.Bool),
        Expression.Lit(Literal.Str("hi"), Type.Str),
        Expression.Lit(Literal.Str("hello"), Type.Str),
        Type.Str)),
      Type.Tuple(List(Type.Int, Type.Bool, Type.Str)))
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tuple(List(Value.Int(42), Value.Bool(false), Value.Str("hi"))))(result)
  }

  test("Expression.Tag01") {
    val name = Name.Resolved(List("foo", "bar"))
    val ident = ParsedAst.Ident("baz", SourceLocation(None, 0, 0))
    val tagTpe = Type.Tag(name, ident, Type.Str)
    val enumTpe = Type.Enum(Map("foo.bar.baz" -> tagTpe))
    val input = Expression.Tag(name, ident,
      // if (!(4 != 4)) "hello world" else "asdfasdf"
      Expression.IfThenElse(
        Expression.Unary(
          UnaryOperator.Not,
          Expression.Binary(
            BinaryOperator.NotEqual,
            Expression.Lit(Literal.Int(4), Type.Int),
            Expression.Lit(Literal.Int(4), Type.Int),
            Type.Bool),
          Type.Bool),
        Expression.Lit(Literal.Str("hello world"), Type.Str),
        Expression.Lit(Literal.Str("asdfasdf"), Type.Str),
        Type.Str),
      enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "baz", Value.Str("hello world")))(result)
  }

  test("Expression.Tag02") {
    val name = Name.Resolved(List("Family"))
    val ident = ParsedAst.Ident("NameAndAge", SourceLocation(None, 0, 0))
    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int)))
    val enumTpe = Type.Enum(Map("Family.NameAndAge" -> tagTpe))
    val input = Expression.Tag(name, ident, Expression.Tuple(List(
      Expression.Lit(Literal.Str("James"), Type.Str),
      // 20 + 22
      Expression.Binary(
        BinaryOperator.Plus,
        Expression.Lit(Literal.Int(20), Type.Int),
        Expression.Lit(Literal.Int(22), Type.Int),
        Type.Int)),
      Type.Tuple(List(Type.Str, Type.Int))), enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "NameAndAge", Value.Tuple(List(Value.Str("James"), Value.Int(42)))))(result)
  }

  test("Expression.Tag03a") {
    import ConstantPropTagDefs._
    val input = Expression.Tag(name, identB, Expression.Lit(Literal.Unit, Type.Unit), enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Bot", Value.Unit))(result)
  }

  test("Expression.Tag03b") {
    import ConstantPropTagDefs._
    val input = Expression.Tag(name, identT, Expression.Lit(Literal.Unit, Type.Unit), enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Top", Value.Unit))(result)
  }

  test("Expression.Tag03c") {
    import ConstantPropTagDefs._
    val input = Expression.Tag(name, identV,
      // 123 - 123
      Expression.Binary(
        BinaryOperator.Minus,
        Expression.Lit(Literal.Int(123), Type.Int),
        Expression.Lit(Literal.Int(123), Type.Int),
        Type.Int),
      enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Val", Value.Int(0)))(result)
  }

  test("Expression.Tag03d") {
    import ConstantPropTagDefs._
    val input = Expression.Tag(name, identV,
      // -240
      Expression.Unary(
        UnaryOperator.UnaryMinus,
        Expression.Lit(Literal.Int(240), Type.Int),
        Type.Int),
      enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Val", Value.Int(-240)))(result)
  }

  test("Expression.Tag03e") {
    import ConstantPropTagDefs._
    val input = Expression.Tag(name, identV, Expression.Lit(Literal.Int(1241), Type.Int), enumTpe)
    val result = Interpreter.eval(input, root)
    assertResult(Value.Tag(name, "Val", Value.Int(1241)))(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions - Error                                                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Error01") {
    val input = Expression.Error(SourceLocation(None, 0, 0), Type.Unit)
    intercept[RuntimeException] {
      Interpreter.eval(input, root)
    }
  }

}
