package ca.uwaterloo.flix.runtime

import ca.uwaterloo.flix.api.{Invokable, IValue, Flix}
import ca.uwaterloo.flix.language.ast.Type.Lambda
import ca.uwaterloo.flix.language.ast.TypedAst.{Definition, Expression, Literal, Pattern, Term, FormalArg, Root}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.Compiler
import org.scalatest.FunSuite

import scala.collection.mutable

// NOTE: When writing a new test, call the parser on a string, and then the interpreter on the resulting AST.
// Older tests were written before the front-end was completely implemented, so they had to directly construct ASTs.

class TestInterpreter extends FunSuite {
  val root = Root(Map(), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))

  val loc = SourceLocation.Unknown

  val name01 = Name.Resolved.mk(List("foo.bar"))
  val name02 = Name.Resolved.mk(List("abc.def"))

  def toIdent(s: String): Name.Ident = Name.Ident(SourcePosition.Unknown, s, SourcePosition.Unknown)

  val ident01 = toIdent("x")
  val ident02 = toIdent("y")
  val ident03 = toIdent("z")

  object ConstantPropTagDefs {
    val name = Name.Resolved.mk(List("ConstProp"))
    val identB = toIdent("Bot")
    val identV = toIdent("Val")
    val identT = toIdent("Top")

    val tagTpeB = Type.Tag(name, identB, Type.Unit)
    val tagTpeV = Type.Tag(name, identV, Type.Int32)
    val tagTpeT = Type.Tag(name, identT, Type.Unit)
    val enumTpe = Type.Enum(Name.Resolved.mk("ConstProp"), Map("ConstProp.Bot" -> tagTpeB, "ConstProp.Val" -> tagTpeV, "ConstProp.Top" -> tagTpeT))
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{Unit,Bool,Int8,Int16,Int32,Int64,Str}                       //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Unit") {
    val input = "fn f: () = ()"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.Unit)(result)
  }

  test("Expression.Bool.01") {
    val input = "fn f: Bool = true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Bool.02") {
    val input = "fn f: Bool = false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Int.01") {
    val input = "fn f: Int = 0"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(0))(result)
  }

  test("Expression.Int.02") {
    val input = "fn f: Int = -254542"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(-254542))(result)
  }

  test("Expression.Int.03") {
    val input = "fn f: Int = 45649878"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(45649878))(result)
  }

  test("Expression.Int.04") {
    val input = s"fn f: Int = ${Int.MaxValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MaxValue))(result)
  }

  test("Expression.Int.05") {
    val input = s"fn f: Int = ${Int.MinValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MinValue))(result)
  }

  ignore("Expression.Int8.01") {
    val input = "fn f: Int8 = -105"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(-105))(result)
  }

  ignore("Expression.Int8.02") {
    val input = "fn f: Int8 = 121"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(121))(result)
  }

  ignore("Expression.Int8.03") {
    val input = s"fn f: Int8 = ${Byte.MaxValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(Byte.MaxValue))(result)
  }

  ignore("Expression.Int8.04") {
    val input = s"fn f: Int8 = ${Byte.MinValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt8(Byte.MinValue))(result)
  }

  ignore("Expression.Int16.01") {
    val input = "fn f: Int16 = -5320"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(-5320))(result)
  }

  ignore("Expression.Int16.02") {
    val input = "fn f: Int16 = 4568"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(4568))(result)
  }

  ignore("Expression.Int16.03") {
    val input = s"fn f: Int16 = ${Short.MaxValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(Short.MaxValue))(result)
  }

  ignore("Expression.Int16.04") {
    val input = s"fn f: Int16 = ${Short.MinValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt16(Short.MinValue))(result)
  }

  test("Expression.Int32.01") {
    val input = "fn f: Int32 = -254542"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(-254542))(result)
  }

  test("Expression.Int32.02") {
    val input = "fn f: Int32 = 45649878"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(45649878))(result)
  }

  test("Expression.Int32.03") {
    val input = s"fn f: Int32 = ${Int.MaxValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MaxValue))(result)
  }

  test("Expression.Int32.04") {
    val input = s"fn f: Int32 = ${Int.MinValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(Int.MinValue))(result)
  }

  ignore("Expression.Int64.01") {
    val input = "fn f: Int64 = -254454121542"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(-254454121542L))(result)
  }

  ignore("Expression.Int64.02") {
    val input = "fn f: Int64 = 45641198784545"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(45641198784545L))(result)
  }

  ignore("Expression.Int64.03") {
    val input = s"fn f: Int64 = ${Long.MaxValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(Long.MaxValue))(result)
  }

  ignore("Expression.Int64.04") {
    val input = s"fn f: Int64 = ${Long.MinValue}"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt64(Long.MinValue))(result)
  }

  test("Expression.Str.01") {
    val input = """fn f: Str = """""
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr(""))(result)
  }

  test("Expression.Str.02") {
    val input = """fn f: Str = "Hello World!""""
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr("Hello World!"))(result)
  }

  test("Expression.Str.03") {
    val input = """fn f: Str = "asdf""""
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkStr("asdf"))(result)
  }

  // TODO: Merge Literal.{Tuple,Set} tests with Expression.{Tuple,Set} tests

//  test("Interpreter - Literal.Tuple01") {
//    val input = Expression.Lit(
//      Literal.Tuple(List(Literal.Int(42, loc), Literal.Bool(false, loc), Literal.Str("hi", loc)),
//        Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc),
//      Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.Tuple(Array(Value.mkInt32(42), Value.False, Value.mkStr("hi"))))(result)
//  }
//
//  test("Interpreter - Literal.Tuple02") {
//    val input = Expression.Lit(
//      Literal.Tuple(List(
//        Literal.Int(4, loc),
//        Literal.Tuple(List(Literal.Int(12, loc), Literal.Int(8, loc)),
//          Type.Tuple(List(Type.Int32, Type.Int32)), loc)),
//        Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.Tuple(Array(Value.mkInt32(4), Value.Tuple(Array(Value.mkInt32(12), Value.mkInt32(8))))))(result)
//  }
//
//  test("Interpreter - Literal.Set01") {
//    val input = Expression.Lit(Literal.Set(List(), Type.Set(Type.Int32), loc), Type.Set(Type.Int32), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkSet(Set()))(result)
//  }
//
//  test("Interpreter - Literal.Set02") {
//    val input = Expression.Lit(Literal.Set(List(), Type.Set(Type.Bool), loc), Type.Set(Type.Bool), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkSet(Set()))(result)
//  }
//
//  test("Interpreter - Literal.Set03") {
//    val input = Expression.Lit(Literal.Set(List(
//      Literal.Int(3, loc),
//      Literal.Int(100, loc),
//      Literal.Int(44, loc)
//    ), Type.Set(Type.Int32), loc), Type.Set(Type.Int32), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkSet(Set(3, 100, 44).map(Value.mkInt32)))(result)
//  }
//
//  test("Interpreter - Literal.Set04") {
//    val input = Expression.Lit(Literal.Set(List(
//      Literal.Bool(true, loc)
//    ), Type.Set(Type.Bool), loc), Type.Set(Type.Bool), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkSet(Set(Value.True)))(result)
//  }
//
//  test("Interpreter - Literal.Set05") {
//    val input = Expression.Lit(Literal.Set(
//      List(Literal.Tuple(List(
//        Literal.Int(3, loc),
//        Literal.Str("three", loc)), Type.Tuple(List(Type.Int32, Type.Str)),
//        loc)),
//      Type.Set(Type.Tuple(List(Type.Int32, Type.Str))), loc),
//      Type.Set(Type.Tuple(List(Type.Int32, Type.Str))), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkSet(Set(Value.Tuple(Array(Value.mkInt32(3), Value.mkStr("three"))))))(result)
//  }

  /////////////////////////////////////////////////////////////////////////////
  // LoadExpression and StoreExpression                                      //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: LoadExpression and StoreExpression tests
  // {Load,Store}Expressions are generated, and not explicitly written in a Flix program

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Var                                                          //
  // Tested indirectly by Expression.{Lambda, Let}.                          //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Combine Expression.Var tests with Expression.Lambda and Expression.Let
  // Skip for now, come back later.

//  test("Interpreter - Expression.Var01") {
//    val input = Expression.Lit(Literal.Str("hello", loc), Type.Str, loc)
//    val env = mutable.Map(ident01.name -> Value.False)
//    val result = Interpreter.eval(input, root, env)
//    assertResult(Value.mkStr("hello"))(result)
//  }
//
//  test("Interpreter - Expression.Var02") {
//    val input = Expression.Var(ident01, Type.Int32, loc)
//    val env = mutable.Map(ident01.name -> Value.mkInt32(5))
//    val result = Interpreter.eval(input, root, env)
//    assertResult(Value.mkInt32(5))(result)
//  }
//
//  test("Interpreter - Expression.Var03") {
//    val input = Expression.Var(ident01, Type.Bool, loc)
//    val env = mutable.Map(ident01.name -> Value.False)
//    val result = Interpreter.eval(input, root, env)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - Expression.Var04") {
//    val input = Expression.Var(ident02, Type.Str, loc)
//    val env = mutable.Map(ident01.name -> Value.mkStr("foo"), ident02.name -> Value.mkStr("bar"))
//    val result = Interpreter.eval(input, root, env)
//    assertResult(Value.mkStr("bar"))(result)
//  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Ref                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Ref.01") {
    val input =
      """namespace Foo::Bar {
        |  fn x: Bool = false
        |  fn f: Str = "foo"
        |}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("Foo::Bar::f"))
    assertResult(Value.mkStr("foo"))(result)
  }

  // TODO: Does it really have to be x() instead of x?
  test("Expression.Ref.02") {
    val input =
      """namespace Foo {
        |  fn x: Int = 5
        |  fn f: Int = x()
        |}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("Foo::f"))
    assertResult(Value.mkInt32(5))(result)
  }

  test("Expression.Ref.03") {
    val input =
      """namespace Foo {
        |  fn x: Bool = true
        |  fn y: Bool = false
        |  fn f: Bool = y()
        |}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("Foo::f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Ref.04") {
    val input =
      """namespace Foo {
        |  fn x: Str = "hello"
        |}
        |namespace Bar {
        |  fn x: Str = Foo::x()
        |}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("Bar::x"))
    assertResult(Value.mkStr("hello"))(result)
  }

  // TODO: Lambda, Hook, Closure, Apply, Apply3
  // Combine Expression.Var tests with Expression.Lambda and Expression.Let
  // Skip for now, come back later.

//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Lambda and Apply                                          //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.Lambda01") {
//    // () => false
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc), Type.Lambda(List(), Type.Bool), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (() => false)()
//    val apply = Expression.Apply(lambda, List(), Type.Bool, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.False)(value)
//  }
//
//  test("Interpreter - Expression.Lambda02") {
//    // x => 3
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)), Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x => 3)(4)
//    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(4, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(3))(value)
//  }
//
//  test("Interpreter - Expression.Lambda03") {
//    // x => x
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)), Expression.Var(ident01, Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x => x)(5)
//    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(5, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(5))(value)
//  }
//
//  test("Interpreter - Expression.Lambda04") {
//    // x => 1 + 2
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Lit(Literal.Int(1, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x => 1 + 2)(42)
//    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(42, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(3))(value)
//  }
//
//  test("Interpreter - Expression.Lambda05") {
//    // x => x + 2
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x => x + 2)(100)
//    val apply = Expression.Apply(lambda, List(Expression.Lit(Literal.Int(100, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(102))(value)
//  }
//
//  test("Interpreter - Expression.Lambda06") {
//    // (x, y) => x + y
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32), FormalArg(ident02, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Var(ident02, Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x, y => x + y)(3, 4)
//    val apply = Expression.Apply(lambda, List(
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(4, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(7))(value)
//  }
//
//  test("Interpreter - Expression.Lambda07") {
//    // (x, y) => if (x) then true else y
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Bool), FormalArg(ident02, Type.Bool)),
//      Expression.IfThenElse(
//        Expression.Var(ident01, Type.Bool, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//        Expression.Var(ident02, Type.Bool, loc),
//        Type.Bool, loc),
//      Type.Lambda(List(Type.Bool, Type.Bool), Type.Bool), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // ((x, y) => if (x) then true else y)(false, true)
//    val apply = Expression.Apply(lambda, List(
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      Type.Bool, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.True)(value)
//  }
//
//  test("Interpreter - Expression.Lambda08") {
//    // (x, y, z) => x + (y + z)
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32), FormalArg(ident02, Type.Int32), FormalArg(ident03, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident02, Type.Int32, loc),
//          Expression.Var(ident03, Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x, y, z => x + (y + z))(2, 42, 5)
//    val apply = Expression.Apply(lambda, List(
//      Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(42, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(5, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(49))(value)
//  }
//
//  test("Interpreter - Expression.Lambda09") {
//    // x => (y => x + y)
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Lambda(
//        Ast.Annotations(List.empty),
//        List(FormalArg(ident02, Type.Int32)),
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident01, Type.Int32, loc),
//          Expression.Var(ident02, Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Lambda(List(Type.Int32), Type.Int32), loc),
//      Type.Lambda(List(Type.Int32), Type.Lambda(List(Type.Int32), Type.Int32)), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x => (y => x + y)(3)(4)
//    val apply = Expression.Apply(
//      Expression.Apply(lambda, List(Expression.Lit(Literal.Int(3, loc), Type.Int32, loc)),
//        Type.Lambda(List(Type.Int32), Type.Int32), loc),
//      List(Expression.Lit(Literal.Int(4, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(7))(value)
//  }
//
//  test("Interpreter - Expression.Lambda10") {
//    // x, y => x(y)
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Lambda(List(Type.Int32), Type.Int32)), FormalArg(ident02, Type.Int32)),
//      Expression.Apply(
//        Expression.Var(ident01, Type.Lambda(List(Type.Int32), Type.Int32), loc),
//        List(Expression.Var(ident02, Type.Int32, loc)),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Lambda(List(Type.Int32), Type.Int32), Type.Int32), Type.Int32), loc)
//    val expected = Value.Closure(lambda.args.map(_.ident.name).toArray, lambda.body, mutable.Map())
//    val closure = Interpreter.eval(lambda, root)
//    assertResult(expected)(closure)
//
//    // (x, y => x(y))((x => x + 1), 5)
//    val apply = Expression.Apply(lambda, List(
//      Expression.Lambda(
//        Ast.Annotations(List.empty),
//        List(FormalArg(ident01, Type.Int32)),
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident01, Type.Int32, loc),
//          Expression.Lit(Literal.Int(1, loc), Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Lambda(List(Type.Int32), Type.Int32), loc),
//      Expression.Lit(Literal.Int(5, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.eval(apply, root)
//    assertResult(Value.mkInt32(6))(value)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Hook and Apply                                            //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.Hook01") {
//    val input =
//      s"""namespace A {
//          |  fn f(x: Int): Bool = g(x)
//          |  fn x(): Bool = f(42)
//          |};
//       """.stripMargin
//    var executed = false
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
//    flix
//      .addStr(input)
//      .addHook("A::g", tpe, new Invokable {
//        def apply(args: Array[IValue]): IValue = {
//          executed = true
//          flix.mkTrue
//        }
//      })
//    val model = flix.solve()
//    val result = model.get.constants(Name.Resolved.mk("A::x"))
//    assert(executed)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - Expression.Hook02") {
//    val input =
//      s"""namespace A {
//          |  fn f(x: Int, y: Int): Int = g(x, y)
//          |  fn x(): Int = f(40, 2)
//          |};
//       """.stripMargin
//    var executed = false
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type), flix.mkInt32Type)
//    flix
//      .addStr(input)
//      .addHook("A::g", tpe, new Invokable {
//        def apply(args: Array[IValue]): IValue = {
//          executed = true
//          flix.mkInt32(args(0).getInt32 + args(1).getInt32)
//        }
//      })
//    val model = flix.solve()
//    val result = model.get.constants(Name.Resolved.mk("A::x"))
//    assert(executed)
//    assertResult(Value.mkInt32(42))(result)
//  }
//
//  test("Interpreter - Expression.Hook03") {
//    val input =
//      s"""namespace A {
//          |  fn f(x: Int): Int = g()
//          |  fn x(): Int = f(42)
//          |};
//       """.stripMargin
//    var executed = false
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(), flix.mkInt32Type)
//    flix
//      .addStr(input)
//      .addHook("A::g", tpe, new Invokable {
//        def apply(args: Array[IValue]): IValue = {
//          executed = true
//          flix.mkInt32(123)
//        }
//      })
//    val model = flix.solve()
//    val result = model.get.constants(Name.Resolved.mk("A::x"))
//    assert(executed)
//    assertResult(Value.mkInt32(123))(result)
//  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Unary                                                        //
  // UnaryOperator.{LogicalNot,Plus,Minus,BitwiseNegate}                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Unary - UnaryOperator.LogicalNot.01") {
    val input = "fn f: Bool = !true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Unary - UnaryOperator.LogicalNot.02") {
    val input = "fn f: Bool = !false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Unary - UnaryOperator.Plus.01") {
    val input =
      s"""fn f01: Int = +0
         |fn f02: Int = +36000
         |fn f03: Int = +(-36000)
         |fn f04: Int = +${Int.MaxValue}
         |fn f05: Int = +${Int.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(36000))(result02)
    assertResult(Value.mkInt32(-36000))(result03)
    assertResult(Value.mkInt32(Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.02") {
    val input =
      s"""fn f01: Int8 = +0
         |fn f02: Int8 = +36
         |fn f03: Int8 = +(-36)
         |fn f04: Int8 = +${Byte.MaxValue}
         |fn f05: Int8 = +${Byte.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0))(result01)
    assertResult(Value.mkInt8(36))(result02)
    assertResult(Value.mkInt8(-36))(result03)
    assertResult(Value.mkInt8(Byte.MaxValue))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.03") {
    val input =
      s"""fn f01: Int16 = +0
         |fn f02: Int16 = +3600
         |fn f03: Int16 = +(-3600)
         |fn f04: Int16 = +${Short.MaxValue}
         |fn f05: Int16 = +${Short.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0))(result01)
    assertResult(Value.mkInt16(3600))(result02)
    assertResult(Value.mkInt16(-3600))(result03)
    assertResult(Value.mkInt16(Short.MaxValue))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  test("Expression.Unary - UnaryOperator.Plus.04") {
    val input =
      s"""fn f01: Int32 = +0
         |fn f02: Int32 = +36000
         |fn f03: Int32 = +(-36000)
         |fn f04: Int32 = +${Int.MaxValue}
         |fn f05: Int32 = +${Int.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(36000))(result02)
    assertResult(Value.mkInt32(-36000))(result03)
    assertResult(Value.mkInt32(Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Plus.05") {
    val input =
      s"""fn f01: Int64 = +0
         |fn f02: Int64 = +3600000000
         |fn f03: Int64 = +(-3600000000)
         |fn f04: Int64 = +${Long.MaxValue}
         |fn f05: Int64 = +${Long.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0))(result01)
    assertResult(Value.mkInt64(3600000000L))(result02)
    assertResult(Value.mkInt64(-3600000000L))(result03)
    assertResult(Value.mkInt64(Long.MaxValue))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Unary - UnaryOperator.Minus.01") {
    val input =
      s"""fn f01: Int = -0
         |fn f02: Int = -36000
         |fn f03: Int = -(-36000)
         |fn f04: Int = -${Int.MaxValue}
         |fn f05: Int = -${Int.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(-36000))(result02)
    assertResult(Value.mkInt32(36000))(result03)
    assertResult(Value.mkInt32(-Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.02") {
    val input =
      s"""fn f01: Int8 = -0
          |fn f02: Int8 = -36
          |fn f03: Int8 = -(-36)
          |fn f04: Int8 = -${Byte.MaxValue}
          |fn f05: Int8 = -${Byte.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0))(result01)
    assertResult(Value.mkInt8(-36))(result02)
    assertResult(Value.mkInt8(36))(result03)
    assertResult(Value.mkInt8(-Byte.MaxValue))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.03") {
    val input =
      s"""fn f01: Int16 = -0
          |fn f02: Int16 = -3600
          |fn f03: Int16 = -(-3600)
          |fn f04: Int16 = -${Short.MaxValue}
          |fn f05: Int16 = -${Short.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0))(result01)
    assertResult(Value.mkInt16(-3600))(result02)
    assertResult(Value.mkInt16(3600))(result03)
    assertResult(Value.mkInt16(-Short.MaxValue))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  test("Expression.Unary - UnaryOperator.Minus.04") {
    val input =
      s"""fn f01: Int32 = -0
         |fn f02: Int32 = -36000
         |fn f03: Int32 = -(-36000)
         |fn f04: Int32 = -${Int.MaxValue}
         |fn f05: Int32 = -${Int.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(-36000))(result02)
    assertResult(Value.mkInt32(36000))(result03)
    assertResult(Value.mkInt32(-Int.MaxValue))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Unary - UnaryOperator.Minus.05") {
    val input =
      s"""fn f01: Int64 = -0
          |fn f02: Int64 = -3600000000
          |fn f03: Int64 = -(-3600000000)
          |fn f04: Int64 = -${Long.MaxValue}
          |fn f05: Int64 = -${Long.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0))(result01)
    assertResult(Value.mkInt64(-3600000000L))(result02)
    assertResult(Value.mkInt64(3600000000L))(result03)
    assertResult(Value.mkInt64(-Long.MaxValue))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.01") {
    val input =
      s"""fn f01: Int = ~0
         |fn f02: Int = ~1
         |fn f03: Int = ~(-1)
         |fn f04: Int = ~36000
         |fn f05: Int = ~(-36000)
         |fn f06: Int = ~${Int.MaxValue}
         |fn f07: Int = ~${Int.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt32(-1))(result01)
    assertResult(Value.mkInt32(-2))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-36001))(result04)
    assertResult(Value.mkInt32(35999))(result05)
    assertResult(Value.mkInt32(Int.MinValue))(result06)
    assertResult(Value.mkInt32(Int.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.02") {
    val input =
      s"""fn f01: Int8 = ~0
          |fn f02: Int8 = ~1
          |fn f03: Int8 = ~(-1)
          |fn f04: Int8 = ~42
          |fn f05: Int8 = ~(-42)
          |fn f06: Int8 = ~${Byte.MaxValue}
          |fn f07: Int8 = ~${Byte.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt8(-1))(result01)
    assertResult(Value.mkInt8(-2))(result02)
    assertResult(Value.mkInt8(0))(result03)
    assertResult(Value.mkInt8(-43))(result04)
    assertResult(Value.mkInt8(41))(result05)
    assertResult(Value.mkInt8(Byte.MinValue))(result06)
    assertResult(Value.mkInt8(Byte.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.03") {
    val input =
      s"""fn f01: Int16 = ~0
          |fn f02: Int16 = ~1
          |fn f03: Int16 = ~(-1)
          |fn f04: Int16 = ~420
          |fn f05: Int16 = ~(-420)
          |fn f06: Int16 = ~${Short.MaxValue}
          |fn f07: Int16 = ~${Short.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt16(-1))(result01)
    assertResult(Value.mkInt16(-2))(result02)
    assertResult(Value.mkInt16(0))(result03)
    assertResult(Value.mkInt16(-421))(result04)
    assertResult(Value.mkInt16(419))(result05)
    assertResult(Value.mkInt16(Short.MinValue))(result06)
    assertResult(Value.mkInt16(Short.MaxValue))(result07)
  }

  test("Expression.Unary - UnaryOperator.BitwiseNegate.04") {
    val input =
      s"""fn f01: Int32 = ~0
         |fn f02: Int32 = ~1
         |fn f03: Int32 = ~(-1)
         |fn f04: Int32 = ~36000
         |fn f05: Int32 = ~(-36000)
         |fn f06: Int32 = ~${Int.MaxValue}
         |fn f07: Int32 = ~${Int.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt32(-1))(result01)
    assertResult(Value.mkInt32(-2))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-36001))(result04)
    assertResult(Value.mkInt32(35999))(result05)
    assertResult(Value.mkInt32(Int.MinValue))(result06)
    assertResult(Value.mkInt32(Int.MaxValue))(result07)
  }

  ignore("Expression.Unary - UnaryOperator.BitwiseNegate.05") {
    val input =
      s"""fn f01: Int64 = ~0
          |fn f02: Int64 = ~1
          |fn f03: Int64 = ~(-1)
          |fn f04: Int64 = ~10000000000
          |fn f05: Int64 = ~(-10000000000)
          |fn f06: Int64 = ~${Long.MaxValue}
          |fn f07: Int64 = ~${Long.MinValue}
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    val result07 = model.constants(Name.Resolved.mk("f07"))
    assertResult(Value.mkInt64(-1))(result01)
    assertResult(Value.mkInt64(-2))(result02)
    assertResult(Value.mkInt64(0))(result03)
    assertResult(Value.mkInt64(-10000000001L))(result04)
    assertResult(Value.mkInt64(9999999999L))(result05)
    assertResult(Value.mkInt64(Long.MinValue))(result06)
    assertResult(Value.mkInt64(Long.MaxValue))(result07)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Arithmetic)                                          //
  // BinaryOperator.{Plus,Minus,Times,Divide,Modulo}                         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.Plus.01") {
    val input =
      s"""fn f01: Int = ${Int.MaxValue} + 1
         |fn f02: Int = 100000 + 400000
         |fn f03: Int = -400000 + 100000
         |fn f04: Int = -100000 + 400000
         |fn f05: Int = ${Int.MinValue} + -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MinValue))(result01)
    assertResult(Value.mkInt32(500000))(result02)
    assertResult(Value.mkInt32(-300000))(result03)
    assertResult(Value.mkInt32(300000))(result04)
    assertResult(Value.mkInt32(Int.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MaxValue} + 1
         |fn f02: Int8 = 10 + 40
         |fn f03: Int8 = -40 + 10
         |fn f04: Int8 = -10 + 40
         |fn f05: Int8 = ${Byte.MinValue} + -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(Byte.MinValue))(result01)
    assertResult(Value.mkInt8(50))(result02)
    assertResult(Value.mkInt8(-30))(result03)
    assertResult(Value.mkInt8(30))(result04)
    assertResult(Value.mkInt8(Byte.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MaxValue} + 1
         |fn f02: Int16 = 1000 + 4000
         |fn f03: Int16 = -4000 + 1000
         |fn f04: Int16 = -1000 + 4000
         |fn f05: Int16 = ${Short.MinValue} + -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(Short.MinValue))(result01)
    assertResult(Value.mkInt16(5000))(result02)
    assertResult(Value.mkInt16(-3000))(result03)
    assertResult(Value.mkInt16(3000))(result04)
    assertResult(Value.mkInt16(Short.MaxValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Plus.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MaxValue} + 1
         |fn f02: Int32 = 100000 + 400000
         |fn f03: Int32 = -400000 + 100000
         |fn f04: Int32 = -100000 + 400000
         |fn f05: Int32 = ${Int.MinValue} + -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MinValue))(result01)
    assertResult(Value.mkInt32(500000))(result02)
    assertResult(Value.mkInt32(-300000))(result03)
    assertResult(Value.mkInt32(300000))(result04)
    assertResult(Value.mkInt32(Int.MaxValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Plus.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MaxValue} + 1
         |fn f02: Int64 = 10000000000 + 40000000000
         |fn f03: Int64 = -40000000000 + 10000000000
         |fn f04: Int64 = -10000000000 + 40000000000
         |fn f05: Int64 = ${Long.MinValue} + -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(Long.MinValue))(result01)
    assertResult(Value.mkInt64(50000000000L))(result02)
    assertResult(Value.mkInt64(-30000000000L))(result03)
    assertResult(Value.mkInt64(30000000000L))(result04)
    assertResult(Value.mkInt64(Long.MaxValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Minus.01") {
    val input =
      s"""fn f01: Int = ${Int.MinValue} - 1
         |fn f02: Int = 400000 - 100000
         |fn f03: Int = -400000 - 100000
         |fn f04: Int = -100000 - 400000
         |fn f05: Int = ${Int.MaxValue} - -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(300000))(result02)
    assertResult(Value.mkInt32(-500000))(result03)
    assertResult(Value.mkInt32(-500000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MinValue} - 1
         |fn f02: Int8 = 40 - 10
         |fn f03: Int8 = -40 - 10
         |fn f04: Int8 = -10 - 40
         |fn f05: Int8 = ${Byte.MaxValue} - -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(Byte.MaxValue))(result01)
    assertResult(Value.mkInt8(30))(result02)
    assertResult(Value.mkInt8(-50))(result03)
    assertResult(Value.mkInt8(-50))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MinValue} - 1
         |fn f02: Int16 = 4000 - 1000
         |fn f03: Int16 = -4000 - 1000
         |fn f04: Int16 = -1000 - 4000
         |fn f05: Int16 = ${Short.MaxValue} - -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(Short.MaxValue))(result01)
    assertResult(Value.mkInt16(3000))(result02)
    assertResult(Value.mkInt16(-5000))(result03)
    assertResult(Value.mkInt16(-5000))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Minus.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MinValue} - 1
         |fn f02: Int32 = 400000 - 100000
         |fn f03: Int32 = -400000 - 100000
         |fn f04: Int32 = -100000 - 400000
         |fn f05: Int32 = ${Int.MaxValue} - -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(300000))(result02)
    assertResult(Value.mkInt32(-500000))(result03)
    assertResult(Value.mkInt32(-500000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Minus.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MinValue} - 1
         |fn f02: Int64 = 40000000000 - 10000000000
         |fn f03: Int64 = -40000000000 - 10000000000
         |fn f04: Int64 = -10000000000 - 40000000000
         |fn f05: Int64 = ${Long.MaxValue} - -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(Long.MaxValue))(result01)
    assertResult(Value.mkInt64(30000000000L))(result02)
    assertResult(Value.mkInt64(-50000000000L))(result03)
    assertResult(Value.mkInt64(-50000000000L))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Times.01") {
    val input =
      s"""fn f01: Int = ${Int.MaxValue} * 2
         |fn f02: Int = 300 * 200
         |fn f03: Int = -200 * 300
         |fn f04: Int = -200 * -300
         |fn f05: Int = ${Int.MinValue} * -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-2))(result01)
    assertResult(Value.mkInt32(60000))(result02)
    assertResult(Value.mkInt32(-60000))(result03)
    assertResult(Value.mkInt32(60000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MaxValue} * 2
         |fn f02: Int8 = 3 * 2
         |fn f03: Int8 = -2 * 3
         |fn f04: Int8 = -2 * -3
         |fn f05: Int8 = ${Byte.MinValue} * -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(-2))(result01)
    assertResult(Value.mkInt8(6))(result02)
    assertResult(Value.mkInt8(-6))(result03)
    assertResult(Value.mkInt8(6))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MaxValue} * 2
         |fn f02: Int16 = 30 * 20
         |fn f03: Int16 = -20 * 30
         |fn f04: Int16 = -20 * -30
         |fn f05: Int16 = ${Short.MinValue} * -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(-2))(result01)
    assertResult(Value.mkInt16(600))(result02)
    assertResult(Value.mkInt16(-600))(result03)
    assertResult(Value.mkInt16(600))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Times.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MaxValue} * 2
         |fn f02: Int32 = 300 * 200
         |fn f03: Int32 = -200 * 300
         |fn f04: Int32 = -200 * -300
         |fn f05: Int32 = ${Int.MinValue} * -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-2))(result01)
    assertResult(Value.mkInt32(60000))(result02)
    assertResult(Value.mkInt32(-60000))(result03)
    assertResult(Value.mkInt32(60000))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Times.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MaxValue} * 2
         |fn f02: Int64 = 300000 * 200000
         |fn f03: Int64 = -200000 * 300000
         |fn f04: Int64 = -200000 * -300000
         |fn f05: Int64 = ${Long.MinValue} * -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(-2))(result01)
    assertResult(Value.mkInt64(60000000000L))(result02)
    assertResult(Value.mkInt64(-60000000000L))(result03)
    assertResult(Value.mkInt64(60000000000L))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Divide.01") {
    val input =
      s"""fn f01: Int = ${Int.MaxValue} / 1
         |fn f02: Int = 1200000 / 3
         |fn f03: Int = -1200000 / 3
         |fn f04: Int = -3 / 1200000
         |fn f05: Int = ${Int.MinValue} / -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(400000))(result02)
    assertResult(Value.mkInt32(-400000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.02") {
    val input =
      s"""fn f01: Int8 = ${Byte.MaxValue} / 1
         |fn f02: Int8 = 12 / 3
         |fn f03: Int8 = -12 / 3
         |fn f04: Int8 = -3 / 12
         |fn f05: Int8 = ${Byte.MinValue} / -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(Byte.MaxValue))(result01)
    assertResult(Value.mkInt8(4))(result02)
    assertResult(Value.mkInt8(-4))(result03)
    assertResult(Value.mkInt8(0))(result04)
    assertResult(Value.mkInt8(Byte.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.03") {
    val input =
      s"""fn f01: Int16 = ${Short.MaxValue} / 1
         |fn f02: Int16 = 12000 / 3
         |fn f03: Int16 = -12000 / 3
         |fn f04: Int16 = -3 / 12000
         |fn f05: Int16 = ${Short.MinValue} / -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(Short.MaxValue))(result01)
    assertResult(Value.mkInt16(4000))(result02)
    assertResult(Value.mkInt16(-4000))(result03)
    assertResult(Value.mkInt16(0))(result04)
    assertResult(Value.mkInt16(Short.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Divide.04") {
    val input =
      s"""fn f01: Int32 = ${Int.MaxValue} / 1
         |fn f02: Int32 = 1200000 / 3
         |fn f03: Int32 = -1200000 / 3
         |fn f04: Int32 = -3 / 1200000
         |fn f05: Int32 = ${Int.MinValue} / -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(Int.MaxValue))(result01)
    assertResult(Value.mkInt32(400000))(result02)
    assertResult(Value.mkInt32(-400000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(Int.MinValue))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Divide.05") {
    val input =
      s"""fn f01: Int64 = ${Long.MaxValue} / 1
         |fn f02: Int64 = 120000000000 / 3
         |fn f03: Int64 = -120000000000 / 3
         |fn f04: Int64 = -3 / 120000000000
         |fn f05: Int64 = ${Long.MinValue} / -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(Long.MaxValue))(result01)
    assertResult(Value.mkInt64(40000000000L))(result02)
    assertResult(Value.mkInt64(-40000000000L))(result03)
    assertResult(Value.mkInt64(0))(result04)
    assertResult(Value.mkInt64(Long.MinValue))(result05)
  }

  test("Expression.Binary - BinaryOperator.Modulo.01") {
    val input =
      s"""fn f01: Int = 1200000 % 200000
         |fn f02: Int = 1200000 % 500000
         |fn f03: Int = -1200000 % 500000
         |fn f04: Int = 1200000 % -500000
         |fn f05: Int = -1200000 % -500000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(200000))(result02)
    assertResult(Value.mkInt32(-200000))(result03)
    assertResult(Value.mkInt32(200000))(result04)
    assertResult(Value.mkInt32(-200000))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.02") {
    val input =
      s"""fn f01: Int8 = 12 % 2
         |fn f02: Int8 = 12 % 5
         |fn f03: Int8 = -12 % 5
         |fn f04: Int8 = 12 % -5
         |fn f05: Int8 = -12 % -5
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0))(result01)
    assertResult(Value.mkInt8(2))(result02)
    assertResult(Value.mkInt8(-2))(result03)
    assertResult(Value.mkInt8(2))(result04)
    assertResult(Value.mkInt8(-2))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.03") {
    val input =
      s"""fn f01: Int16 = 12000 % 2000
         |fn f02: Int16 = 12000 % 5000
         |fn f03: Int16 = -12000 % 5000
         |fn f04: Int16 = 12000 % -5000
         |fn f05: Int16 = -12000 % -5000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0))(result01)
    assertResult(Value.mkInt16(2000))(result02)
    assertResult(Value.mkInt16(-2000))(result03)
    assertResult(Value.mkInt16(2000))(result04)
    assertResult(Value.mkInt16(-2000))(result05)
  }

  test("Expression.Binary - BinaryOperator.Modulo.04") {
    val input =
      s"""fn f01: Int32 = 1200000 % 200000
         |fn f02: Int32 = 1200000 % 500000
         |fn f03: Int32 = -1200000 % 500000
         |fn f04: Int32 = 1200000 % -500000
         |fn f05: Int32 = -1200000 % -500000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0))(result01)
    assertResult(Value.mkInt32(200000))(result02)
    assertResult(Value.mkInt32(-200000))(result03)
    assertResult(Value.mkInt32(200000))(result04)
    assertResult(Value.mkInt32(-200000))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.Modulo.05") {
    val input =
      s"""fn f01: Int64 = 120000000000 % 20000000000
         |fn f02: Int64 = 120000000000 % 50000000000
         |fn f03: Int64 = -120000000000 % 50000000000
         |fn f04: Int64 = 120000000000 % -50000000000
         |fn f05: Int64 = -120000000000 % -50000000000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0))(result01)
    assertResult(Value.mkInt64(20000000000L))(result02)
    assertResult(Value.mkInt64(-20000000000L))(result03)
    assertResult(Value.mkInt64(20000000000L))(result04)
    assertResult(Value.mkInt64(-20000000000L))(result05)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Comparison)                                          //
  // BinaryOperator.{Less,LessEqual,Greater,GreaterEqual}                    //
  // BinaryOperator.{Equal,NotEqual}                                         //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Tests for Int8, Int16, Int32 (explicit), and Int64

  test("Expression.Binary - BinaryOperator.Less.01") {
    val input =
      s"""fn f01: Bool = 120000 < 30000
         |fn f02: Bool = 30000 < 120000
         |fn f03: Bool = 30000 < 30000
         |fn f04: Bool = -120000 < -30000
         |fn f05: Bool = -30000 < -120000
         |fn f06: Bool = -30000 < -30000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.False)(result06)
  }

  test("Expression.Binary - BinaryOperator.LessEqual.01") {
    val input =
      s"""fn f01: Bool = 120000 <= 30000
         |fn f02: Bool = 30000 <= 120000
         |fn f03: Bool = 30000 <= 30000
         |fn f04: Bool = -120000 <= -30000
         |fn f05: Bool = -30000 <= -120000
         |fn f06: Bool = -30000 <= -30000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  test("Expression.Binary - BinaryOperator.Greater.01") {
    val input =
      s"""fn f01: Bool = 120000 > 30000
         |fn f02: Bool = 30000 > 120000
         |fn f03: Bool = 30000 > 30000
         |fn f04: Bool = -120000 > -30000
         |fn f05: Bool = -30000 > -120000
         |fn f06: Bool = -30000 > -30000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  test("Expression.Binary - BinaryOperator.GreaterEqual.01") {
    val input =
      s"""fn f01: Bool = 120000 >= 30000
         |fn f02: Bool = 30000 >= 120000
         |fn f03: Bool = 30000 >= 30000
         |fn f04: Bool = -120000 >= -30000
         |fn f05: Bool = -30000 >= -120000
         |fn f06: Bool = -30000 >= -30000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.True)(result06)
  }

  test("Expression.Binary - BinaryOperator.Equal.01") {
    val input =
      s"""fn f01: Bool = 120000 == 30000
         |fn f02: Bool = 30000 == 120000
         |fn f03: Bool = 30000 == 30000
         |fn f04: Bool = -120000 == -30000
         |fn f05: Bool = -30000 == -120000
         |fn f06: Bool = -30000 == -30000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.False)(result01)
    assertResult(Value.False)(result02)
    assertResult(Value.True)(result03)
    assertResult(Value.False)(result04)
    assertResult(Value.False)(result05)
    assertResult(Value.True)(result06)
  }

  test("Expression.Binary - BinaryOperator.NotEqual.01") {
    val input =
      s"""fn f01: Bool = 120000 != 30000
         |fn f02: Bool = 30000 != 120000
         |fn f03: Bool = 30000 != 30000
         |fn f04: Bool = -120000 != -30000
         |fn f05: Bool = -30000 != -120000
         |fn f06: Bool = -30000 != -30000
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    val result06 = model.constants(Name.Resolved.mk("f06"))
    assertResult(Value.True)(result01)
    assertResult(Value.True)(result02)
    assertResult(Value.False)(result03)
    assertResult(Value.True)(result04)
    assertResult(Value.True)(result05)
    assertResult(Value.False)(result06)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Logical)                                             //
  // BinaryOperator.{LogicalAnd,LogicalOr,Implication,Biconditional}         //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.LogicalAnd.01") {
    val input = "fn f: Bool = true && true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.02") {
    val input = "fn f: Bool = true && false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.03") {
    val input = "fn f: Bool = false && false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.04") {
    val input = "fn f: Bool = false && true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.05") {
    val input = "fn f: Bool = false && ???: Bool"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalAnd.06") {
    val input = "fn f: Bool = true && ???: Bool"
    intercept[RuntimeException] { new Flix().addStr(input).solve().get }
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.01") {
    val input = "fn f: Bool = true || true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.02") {
    val input = "fn f: Bool = true || false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.03") {
    val input = "fn f: Bool = false || false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.04") {
    val input = "fn f: Bool = false || true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.05") {
    val input = "fn f: Bool = true || ???: Bool"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.LogicalOr.06") {
    val input = "fn f: Bool = false || ???: Bool"
    intercept[RuntimeException] { new Flix().addStr(input).solve().get }
  }

  test("Expression.Binary - BinaryOperator.Implication.01") {
    val input = "fn f: Bool = true ==> true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.02") {
    val input = "fn f: Bool = true ==> false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.03") {
    val input = "fn f: Bool = false ==> false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.04") {
    val input = "fn f: Bool = false ==> true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.05") {
    val input = "fn f: Bool = false ==> ???: Bool"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Implication.06") {
    val input = "fn f: Bool = true ==> ???: Bool"
    intercept[RuntimeException] { new Flix().addStr(input).solve().get }
  }

  test("Expression.Binary - BinaryOperator.Biconditional.01") {
    val input = "fn f: Bool = true <==> true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Biconditional.02") {
    val input = "fn f: Bool = true <==> false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  test("Expression.Binary - BinaryOperator.Biconditional.03") {
    val input = "fn f: Bool = false <==> false"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.True)(result)
  }

  test("Expression.Binary - BinaryOperator.Biconditional.04") {
    val input = "fn f: Bool = false <==> true"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.False)(result)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Binary (Bitwise)                                             //
  // BinaryOperator.{BitwiseAnd,BitwiseOr,BitwiseXor}                        //
  // BinaryOperator.{BitwiseLeftShift,BitwiseRightShift}                     //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Binary - BinaryOperator.BitwiseAnd.01") {
    val input =
      s"""fn f01: Int = 40000 & ${0xFFFFFFFF}
         |fn f02: Int = 40000 & 40000
         |fn f03: Int = 40000 & 0
         |fn f04: Int = ${0xFFFFFFFF} & ${0xFFFFFFFF}
         |fn f05: Int = -1 & -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(40000))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.02") {
    val input =
      s"""fn f01: Int8 = 40 & ${0xFF.toByte}
         |fn f02: Int8 = 40 & 40
         |fn f03: Int8 = 40 & 0
         |fn f04: Int8 = ${0xFF.toByte} & ${0xFF.toByte}
         |fn f05: Int8 = -1 & -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(40))(result01)
    assertResult(Value.mkInt8(40))(result02)
    assertResult(Value.mkInt8(0))(result03)
    assertResult(Value.mkInt8(0xFF.toByte))(result04)
    assertResult(Value.mkInt8(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.03") {
    val input =
      s"""fn f01: Int16 = 400 & ${0xFFFF.toShort}
         |fn f02: Int16 = 400 & 400
         |fn f03: Int16 = 400 & 0
         |fn f04: Int16 = ${0xFFFF.toShort} & ${0xFFFF.toShort}
         |fn f05: Int16 = -1 & -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(400))(result01)
    assertResult(Value.mkInt16(400))(result02)
    assertResult(Value.mkInt16(0))(result03)
    assertResult(Value.mkInt16(0xFFFF.toShort))(result04)
    assertResult(Value.mkInt16(-1))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseAnd.04") {
    val input =
      s"""fn f01: Int32 = 40000 & ${0xFFFFFFFF}
         |fn f02: Int32 = 40000 & 40000
         |fn f03: Int32 = 40000 & 0
         |fn f04: Int32 = ${0xFFFFFFFF} & ${0xFFFFFFFF}
         |fn f05: Int32 = -1 & -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(40000))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseAnd.05") {
    val input =
      s"""fn f01: Int64 = 40000000000 & ${0xFFFFFFFFFFFFFFFFL}
         |fn f02: Int64 = 40000000000 & 40000000000
         |fn f03: Int64 = 40000000000 & 0
         |fn f04: Int64 = ${0xFFFFFFFFFFFFFFFFL} & ${0xFFFFFFFFFFFFFFFFL}
         |fn f05: Int64 = -1 & -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(40000000000L))(result01)
    assertResult(Value.mkInt64(40000000000L))(result02)
    assertResult(Value.mkInt64(0))(result03)
    assertResult(Value.mkInt64(0xFFFFFFFFFFFFFFFFL))(result04)
    assertResult(Value.mkInt64(-1))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.01") {
    val input =
      s"""fn f01: Int = 40000 | ${0xFFFFFFFF}
         |fn f02: Int = 40000 | 40000
         |fn f03: Int = 40000 | 0
         |fn f04: Int = ${0xFFFFFFFF} | ${0xFFFFFFFF}
         |fn f05: Int = -1 | -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0xFFFFFFFF))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.02") {
    val input =
      s"""fn f01: Int8 = 40 | ${0xFF.toByte}
         |fn f02: Int8 = 40 | 40
         |fn f03: Int8 = 40 | 0
         |fn f04: Int8 = ${0xFF.toByte} | ${0xFF.toByte}
         |fn f05: Int8 = -1 | -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(0xFF.toByte))(result01)
    assertResult(Value.mkInt8(40))(result02)
    assertResult(Value.mkInt8(40))(result03)
    assertResult(Value.mkInt8(0xFF.toByte))(result04)
    assertResult(Value.mkInt8(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.03") {
    val input =
      s"""fn f01: Int16 = 400 | ${0xFFFF.toShort}
         |fn f02: Int16 = 400 | 400
         |fn f03: Int16 = 400 | 0
         |fn f04: Int16 = ${0xFFFF.toShort} | ${0xFFFF.toShort}
         |fn f05: Int16 = -1 | -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(0xFFFF.toShort))(result01)
    assertResult(Value.mkInt16(400))(result02)
    assertResult(Value.mkInt16(400))(result03)
    assertResult(Value.mkInt16(0xFF.toByte))(result04)
    assertResult(Value.mkInt16(-1))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseOr.04") {
    val input =
      s"""fn f01: Int32 = 40000 | ${0xFFFFFFFF}
         |fn f02: Int32 = 40000 | 40000
         |fn f03: Int32 = 40000 | 0
         |fn f04: Int32 = ${0xFFFFFFFF} | ${0xFFFFFFFF}
         |fn f05: Int32 = -1 | -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(0xFFFFFFFF))(result01)
    assertResult(Value.mkInt32(40000))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0xFFFFFFFF))(result04)
    assertResult(Value.mkInt32(-1))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseOr.05") {
    val input =
      s"""fn f01: Int64 = 40000000000 | ${0xFFFFFFFFFFFFFFFFL}
         |fn f02: Int64 = 40000000000 | 40000000000
         |fn f03: Int64 = 40000000000 | 0
         |fn f04: Int64 = ${0xFFFFFFFFFFFFFFFFL} | ${0xFFFFFFFFFFFFFFFFL}
         |fn f05: Int64 = -1 | -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(0xFFFFFFFFFFFFFFFFL))(result01)
    assertResult(Value.mkInt64(40000000000L))(result02)
    assertResult(Value.mkInt64(40000000000L))(result03)
    assertResult(Value.mkInt64(0xFFFFFFFFFFFFFFFFL))(result04)
    assertResult(Value.mkInt64(-1))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.01") {
    val input =
      s"""fn f01: Int = 40000 ^ ${0xFFFFFFFF}
         |fn f02: Int = 40000 ^ 40000
         |fn f03: Int = 40000 ^ 0
         |fn f04: Int = ${0xFFFFFFFF} ^ ${0xFFFFFFFF}
         |fn f05: Int = -1 ^ -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-40001))(result01)
    assertResult(Value.mkInt32(0))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.02") {
    val input =
      s"""fn f01: Int8 = 40 ^ ${0xFF.toByte}
         |fn f02: Int8 = 40 ^ 40
         |fn f03: Int8 = 40 ^ 0
         |fn f04: Int8 = ${0xFF.toByte} ^ ${0xFF.toByte}
         |fn f05: Int8 = -1 ^ -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt8(-41))(result01)
    assertResult(Value.mkInt8(0))(result02)
    assertResult(Value.mkInt8(40))(result03)
    assertResult(Value.mkInt8(0))(result04)
    assertResult(Value.mkInt8(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.03") {
    val input =
      s"""fn f01: Int16 = 400 ^ ${0xFFFF.toShort}
         |fn f02: Int16 = 400 ^ 400
         |fn f03: Int16 = 400 ^ 0
         |fn f04: Int16 = ${0xFFFF.toShort} ^ ${0xFFFF.toShort}
         |fn f05: Int16 = -1 ^ -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt16(-401))(result01)
    assertResult(Value.mkInt16(0))(result02)
    assertResult(Value.mkInt16(400))(result03)
    assertResult(Value.mkInt16(0))(result04)
    assertResult(Value.mkInt16(0))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseXor.04") {
    val input =
      s"""fn f01: Int32 = 40000 ^ ${0xFFFFFFFF}
         |fn f02: Int32 = 40000 ^ 40000
         |fn f03: Int32 = 40000 ^ 0
         |fn f04: Int32 = ${0xFFFFFFFF} ^ ${0xFFFFFFFF}
         |fn f05: Int32 = -1 ^ -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt32(-40001))(result01)
    assertResult(Value.mkInt32(0))(result02)
    assertResult(Value.mkInt32(40000))(result03)
    assertResult(Value.mkInt32(0))(result04)
    assertResult(Value.mkInt32(0))(result05)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseXor.05") {
    val input =
      s"""fn f01: Int64 = 40000000000 ^ ${0xFFFFFFFFFFFFFFFFL}
         |fn f02: Int64 = 40000000000 ^ 40000000000
         |fn f03: Int64 = 40000000000 ^ 0
         |fn f04: Int64 = ${0xFFFFFFFFFFFFFFFFL} ^ ${0xFFFFFFFFFFFFFFFFL}
         |fn f05: Int64 = -1 ^ -1
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    val result05 = model.constants(Name.Resolved.mk("f05"))
    assertResult(Value.mkInt64(-40000000001L))(result01)
    assertResult(Value.mkInt64(0))(result02)
    assertResult(Value.mkInt64(40000000000L))(result03)
    assertResult(Value.mkInt64(0))(result04)
    assertResult(Value.mkInt64(0))(result05)
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.01") {
    val input =
      s"""fn f01: Int = ${0x08} << 0
         |fn f02: Int = ${0x08} << 16
         |fn f03: Int = ${0x08} << 28
         |fn f04: Int = ${0x08} << 29
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(0x08))(result01)
    assertResult(Value.mkInt32(0x00080000))(result02)
    assertResult(Value.mkInt32(Int.MinValue))(result03)
    assertResult(Value.mkInt32(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.02") {
    val input =
      s"""fn f01: Int8 = ${0x08} << 0
         |fn f02: Int8 = ${0x08} << 2
         |fn f03: Int8 = ${0x08} << 4
         |fn f04: Int8 = ${0x08} << 5
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt8(0x08))(result01)
    assertResult(Value.mkInt8(0x20))(result02)
    assertResult(Value.mkInt8(Byte.MinValue))(result03)
    assertResult(Value.mkInt8(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.03") {
    val input =
      s"""fn f01: Int16 = ${0x08} << 0
         |fn f02: Int16 = ${0x08} << 8
         |fn f03: Int16 = ${0x08} << 12
         |fn f04: Int16 = ${0x08} << 13
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt16(0x08))(result01)
    assertResult(Value.mkInt16(0x0800))(result02)
    assertResult(Value.mkInt16(Short.MinValue))(result03)
    assertResult(Value.mkInt16(0))(result04)
  }

  test("Expression.Binary - BinaryOperator.BitwiseLeftShift.04") {
    val input =
      s"""fn f01: Int32 = ${0x08} << 0
         |fn f02: Int32 = ${0x08} << 16
         |fn f03: Int32 = ${0x08} << 28
         |fn f04: Int32 = ${0x08} << 29
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(0x08))(result01)
    assertResult(Value.mkInt32(0x00080000))(result02)
    assertResult(Value.mkInt32(Int.MinValue))(result03)
    assertResult(Value.mkInt32(0))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseLeftShift.05") {
    val input =
      s"""fn f01: Int64 = ${0x08} << 0
         |fn f02: Int64 = ${0x08} << 32
         |fn f03: Int64 = ${0x08} << 60
         |fn f04: Int64 = ${0x08} << 61
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt64(0x08))(result01)
    assertResult(Value.mkInt64(0x0000000800000000L))(result02)
    assertResult(Value.mkInt64(Long.MinValue))(result03)
    assertResult(Value.mkInt64(0))(result04)
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.01") {
    val input =
      s"""fn f01: Int = 120000 >> 0
         |fn f02: Int = 120000 >> 2
         |fn f03: Int = 120000 >> 31
         |fn f04: Int = -120000 >> 2
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(120000))(result01)
    assertResult(Value.mkInt32(30000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-30000))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.02") {
    val input =
      s"""fn f01: Int8 = 120 >> 0
         |fn f02: Int8 = 120 >> 2
         |fn f03: Int8 = 120 >> 7
         |fn f04: Int8 = -120 >> 2
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt8(120))(result01)
    assertResult(Value.mkInt8(30))(result02)
    assertResult(Value.mkInt8(0))(result03)
    assertResult(Value.mkInt8(-30))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.03") {
    val input =
      s"""fn f01: Int16 = 12000 >> 0
         |fn f02: Int16 = 12000 >> 2
         |fn f03: Int16 = 12000 >> 15
         |fn f04: Int16 = -12000 >> 2
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt16(12000))(result01)
    assertResult(Value.mkInt16(3000))(result02)
    assertResult(Value.mkInt16(0))(result03)
    assertResult(Value.mkInt16(-3000))(result04)
  }

  test("Expression.Binary - BinaryOperator.BitwiseRightShift.04") {
    val input =
      s"""fn f01: Int32 = 120000 >> 0
         |fn f02: Int32 = 120000 >> 2
         |fn f03: Int32 = 120000 >> 31
         |fn f04: Int32 = -120000 >> 2
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt32(120000))(result01)
    assertResult(Value.mkInt32(30000))(result02)
    assertResult(Value.mkInt32(0))(result03)
    assertResult(Value.mkInt32(-30000))(result04)
  }

  ignore("Expression.Binary - BinaryOperator.BitwiseRightShift.05") {
    val input =
      s"""fn f01: Int64 = 12000000000 >> 0
         |fn f02: Int64 = 12000000000 >> 2
         |fn f03: Int64 = 12000000000 >> 63
         |fn f04: Int64 = -12000000000 >> 2
       """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("f01"))
    val result02 = model.constants(Name.Resolved.mk("f02"))
    val result03 = model.constants(Name.Resolved.mk("f03"))
    val result04 = model.constants(Name.Resolved.mk("f04"))
    assertResult(Value.mkInt64(12000000000L))(result01)
    assertResult(Value.mkInt64(3000000000L))(result02)
    assertResult(Value.mkInt64(0))(result03)
    assertResult(Value.mkInt64(-3000000000L))(result04)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.IfThenElse                                                   //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.IfThenElse.01") {
    val input = "fn f: Int = if (false) 42 + 10 else 42 - 10"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(32))(result)
  }

  test("Expression.IfThenElse.02") {
    val input = "fn f: Int = if (true) 42 + 10 else 42 - 10"
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkInt32(52))(result)
  }

  test("Expression.IfThenElse.03") {
    val input =
      """fn f(x: Bool): Int = if (x) (if (false) 1 else 2) else (if (true) 3 else 4)
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(2))(result01)
    assertResult(Value.mkInt32(3))(result02)
  }

  test("Expression.IfThenElse.04") {
    val input =
      """fn f(x: Bool): Int = if (if (!x) true else false) 1234 else 5678
        |fn g01: Int = f(true)
        |fn g02: Int = f(false)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(5678))(result01)
    assertResult(Value.mkInt32(1234))(result02)
  }

  test("Expression.IfThenElse.05") {
    val input =
      """fn f(x: Bool, y: Bool): Int = if (x && y) 1234 else 5678
        |fn g01: Int = f(true, true)
        |fn g02: Int = f(false, true)
        |fn g03: Int = f(true, false)
        |fn g04: Int = f(false, false)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(5678))(result02)
    assertResult(Value.mkInt32(5678))(result03)
    assertResult(Value.mkInt32(5678))(result04)
  }

  test("Expression.IfThenElse.06") {
    val input =
      """fn f(x: Bool, y: Bool): Int = if (x || y) 1234 else 5678
        |fn g01: Int = f(true, true)
        |fn g02: Int = f(false, true)
        |fn g03: Int = f(true, false)
        |fn g04: Int = f(false, false)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    val result03 = model.constants(Name.Resolved.mk("g03"))
    val result04 = model.constants(Name.Resolved.mk("g04"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(1234))(result02)
    assertResult(Value.mkInt32(1234))(result03)
    assertResult(Value.mkInt32(5678))(result04)
  }

  ignore("Expression.IfThenElse.07") {
    val input =
      """fn f(x: Int8, y: Int8): Int8 = if (x < y) 12 else 56
        |fn g01: Int8 = f(5, 24)
        |fn g02: Int8 = f(5, 5)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt8(12))(result01)
    assertResult(Value.mkInt8(56))(result02)
  }

  ignore("Expression.IfThenElse.08") {
    val input =
      """fn f(x: Int16, y: Int16): Int16 = if (x <= y) 1234 else 5678
        |fn g01: Int16 = f(500, 500)
        |fn g02: Int16 = f(500, 200)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt16(1234))(result01)
    assertResult(Value.mkInt16(5678))(result02)
  }

  test("Expression.IfThenElse.09") {
    val input =
      """fn f(x: Int32, y: Int32): Int32 = if (x > y) 12341234 else 56785678
        |fn g01: Int32 = f(2400000, 500000)
        |fn g02: Int32 = f(500000, 500000)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(12341234))(result01)
    assertResult(Value.mkInt32(56785678))(result02)
  }

  ignore("Expression.IfThenElse.10") {
    val input =
      """fn f(x: Int64, y: Int64): Int64 = if (x >= y) 123412341234 else 567856785678
        |fn g01: Int64 = f(50000000000, 50000000000)
        |fn g02: Int64 = f(20000000000, 50000000000)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt64(123412341234L))(result01)
    assertResult(Value.mkInt64(567856785678L))(result02)
  }

  test("Expression.IfThenElse.11") {
    val input =
      """fn f(x: Int, y: Int): Int = if (x == y) 1234 else 5678
        |fn g01: Int = f(5, 5)
        |fn g02: Int = f(2, 5)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(5678))(result02)
  }

  test("Expression.IfThenElse.12") {
    val input =
      """fn f(x: Int, y: Int): Int = if (x != y) 1234 else 5678
        |fn g01: Int = f(2, 5)
        |fn g02: Int = f(5, 5)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkInt32(1234))(result01)
    assertResult(Value.mkInt32(5678))(result02)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Let                                                          //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Combine Expression.Var tests with Expression.Lambda and Expression.Let
  // Skip for now, come back later.

  /////////////////////////////////////////////////////////////////////////////
  // Expression.{CheckTag,GetTagValue}                                       //
  // Tested indirectly by pattern matching.                                  //
  /////////////////////////////////////////////////////////////////////////////

  // TODO: Come back to this later

  /////////////////////////////////////////////////////////////////////////////
  // Expression.Tag                                                          //
  /////////////////////////////////////////////////////////////////////////////

  test("Expression.Tag.01") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: ConstProp = ConstProp.Top
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Top", Value.Unit))(result)
  }

  test("Expression.Tag.02") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: ConstProp = ConstProp.Val(42)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Val", Value.mkInt32(42)))(result)
  }

  test("Expression.Tag.03") {
    val input =
      """enum ConstProp { case Top, case Val(Int), case Bot }
        |fn f: ConstProp = ConstProp.Bot
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("ConstProp"), "Bot", Value.Unit))(result)
  }

  test("Expression.Tag.04") {
    val input =
      """enum Val { case Val(Bool) }
        |fn f: Val = Val.Val(true)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.True))(result)
  }

  test("Expression.Tag.05") {
    val input =
      """enum Val { case Val(Bool) }
        |fn f(x: Bool): Val = Val.Val(x)
        |fn g01: Val = f(true)
        |fn g02: Val = f(false)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result01 = model.constants(Name.Resolved.mk("g01"))
    val result02 = model.constants(Name.Resolved.mk("g02"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.True))(result01)
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.False))(result02)
  }

  test("Expression.Tag.06") {
    val input =
      """enum Val { case Val(Str) }
        |fn f: Val = Val.Val("hi")
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkStr("hi")))(result)
  }

  test("Expression.Tag.07") {
    val input =
      """enum Val { case Val(Int, Str) }
        |fn f: Val = Val.Val(1, "one")
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.Tuple(Array(Value.mkInt32(1), "one"))))(result)
  }

  test("Expression.Tag.08") {
    val input =
      """enum Val { case Val(Str) }
        |fn f: Val = Val.Val(if (!(4 != 4)) "foo" else "bar")
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkStr("foo")))(result)
  }

  test("Expression.Tag.09") {
    val input =
      """enum Val { case Val(Str, Int) }
        |fn f: Val = Val.Val("ABC", 20 + 22)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.Tuple(Array("ABC", Value.mkInt32(42)))))(result)
  }

  ignore("Expression.Tag.10") {
    val input =
      """enum Val { case Val(Int8) }
        |fn f: Val = Val.Val(32)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt8(32)))(result)
  }

  ignore("Expression.Tag.11") {
    val input =
      """enum Val { case Val(Int16) }
        |fn f: Val = Val.Val(3200)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt16(3200)))(result)
  }

  ignore("Expression.Tag.12") {
    val input =
      """enum Val { case Val(Int64) }
        |fn f: Val = Val.Val(320000000000)
      """.stripMargin
    val model = new Flix().addStr(input).solve().get
    val result = model.constants(Name.Resolved.mk("f"))
    assertResult(Value.mkTag(Name.Resolved.mk("Val"), "Val", Value.mkInt64(320000000000L)))(result)
  }

//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Switch                                                    //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Switch01") {
//    val input =
//      """fn f: Int = switch {
//        |  case -42 < 0 => 1
//        |  case -42 > 0 => 2
//        |  case true    => 3
//        |}
//      """.stripMargin
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(1))(result)
//  }
//
//  test("Interpreter - Switch02") {
//    val input =
//      """fn f: Int = switch {
//        |  case 42 < 0 => 1
//        |  case 42 > 0 => 2
//        |  case true   => 3
//        |}
//      """.stripMargin
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(2))(result)
//  }
//
//  test("Interpreter - Switch03") {
//    val input =
//      """fn f: Int = switch {
//        |  case 0 < 0 => 1
//        |  case 0 > 0 => 2
//        |  case true  => 3
//        |}
//      """.stripMargin
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(3))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Let                                                       //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.Let01") {
//    // let x = true in 42
//    val input = Expression.Let(ident01, Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Int(42, loc), Type.Int32, loc), Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(42))(result)
//  }
//
//  test("Interpreter - Expression.Let02") {
//    // let x = 24 in x
//    val input = Expression.Let(ident01, Expression.Lit(Literal.Int(24, loc), Type.Int32, loc),
//      Expression.Var(ident01, Type.Int32, loc), Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(24))(result)
//  }
//
//  test("Interpreter - Expression.Let03") {
//    // let x = 1 in x + 2
//    val input = Expression.Let(ident01, Expression.Lit(Literal.Int(1, loc), Type.Int32, loc),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(3))(result)
//  }
//
//  test("Interpreter - Expression.Let04") {
//    // let x = false in if x then "abc" else "xyz"
//    val input = Expression.Let(ident01, Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Expression.IfThenElse(
//        Expression.Var(ident01, Type.Bool, loc),
//        Expression.Lit(Literal.Str("abc", loc), Type.Str, loc),
//        Expression.Lit(Literal.Str("xyz", loc), Type.Str, loc),
//        Type.Str, loc),
//      Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("xyz"))(result)
//  }
//
//  test("Interpreter - Expression.Let05") {
//    // let x = 14 - 3 in x + 2
//    val input = Expression.Let(ident01,
//      Expression.Binary(
//        BinaryOperator.Minus,
//        Expression.Lit(Literal.Int(14, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(13))(result)
//  }
//
//  test("Interpreter - Expression.Let06") {
//    // let x = 14 - 3 in let y = 2 * 4 in x + y
//    val input = Expression.Let(ident01,
//      Expression.Binary(
//        BinaryOperator.Minus,
//        Expression.Lit(Literal.Int(14, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Expression.Let(ident02,
//        Expression.Binary(
//          BinaryOperator.Times,
//          Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//          Expression.Lit(Literal.Int(4, loc), Type.Int32, loc),
//          Type.Int32, loc),
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident01, Type.Int32, loc),
//          Expression.Var(ident02, Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(19))(result)
//  }
//
//  test("Interpreter - Expression.Let07") {
//    // let x = 1 in let y = x + 2 in let z = y + 3 in z
//    val input = Expression.Let(ident01,
//      Expression.Lit(Literal.Int(1, loc), Type.Int32, loc),
//      Expression.Let(ident02,
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident01, Type.Int32, loc),
//          Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//          Type.Int32, loc),
//        Expression.Let(ident03,
//          Expression.Binary(
//            BinaryOperator.Plus,
//            Expression.Var(ident02, Type.Int32, loc),
//            Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//            Type.Int32, loc),
//          Expression.Var(ident03, Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(6))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Match                                                     //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Pattern.Wildcard01") {
//    // Unit match { case _ => 11 }
//    val rules = List((Pattern.Wildcard(Type.Int32, loc), Expression.Lit(Literal.Int(11, loc), Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Unit(loc), Type.Unit, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(11))(result)
//  }
//
//  test("Interpreter - Pattern.Var01") {
//    // 3 match { case x => x }
//    val rules = List((Pattern.Var(ident01, Type.Int32, loc), Expression.Var(ident01, Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Int(3, loc), Type.Int32, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(3))(result)
//  }
//
//  test("Interpreter - Pattern.Var02") {
//    // 3 match { case x => x + 11 }
//    val rules = List((Pattern.Var(ident01, Type.Int32, loc), Expression.Binary(
//      BinaryOperator.Plus,
//      Expression.Var(ident01, Type.Int32, loc),
//      Expression.Lit(Literal.Int(11, loc), Type.Int32, loc),
//      Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Int(3, loc), Type.Int32, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(14))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Unit01") {
//    // Unit match { case Unit => true }
//    val rules = List((Pattern.Lit(Literal.Unit(loc), Type.Unit, loc), Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Unit(loc), Type.Unit, loc), rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Bool01") {
//    // true match { case true => 30 }
//    val rules = List((Pattern.Lit(Literal.Bool(true, loc), Type.Bool, loc), Expression.Lit(Literal.Int(30, loc), Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(30))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Bool02") {
//    // true match { case false => 0; case _ => 1 }
//    val rules = List(
//      (Pattern.Lit(Literal.Bool(false, loc), Type.Bool, loc), Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)),
//      (Pattern.Wildcard(Type.Bool, loc), Expression.Lit(Literal.Int(1, loc), Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(1))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Int01") {
//    // 87 match { case 87 => 1 }
//    val rules = List((Pattern.Lit(Literal.Int(87, loc), Type.Int32, loc), Expression.Lit(Literal.Int(1, loc), Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Int(87, loc), Type.Int32, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(1))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Int02") {
//    // 87 match { case 86 => "foo"; case _ => "bar" }
//    val rules = List(
//      (Pattern.Lit(Literal.Int(86, loc), Type.Int32, loc), Expression.Lit(Literal.Str("foo", loc), Type.Str, loc)),
//      (Pattern.Wildcard(Type.Int32, loc), Expression.Lit(Literal.Str("bar", loc), Type.Str, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Int(87, loc), Type.Int32, loc), rules, Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("bar"))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Str01") {
//    // "hello" match { case "hello" => "world" }
//    val rules = List(
//      (Pattern.Lit(Literal.Str("hello", loc), Type.Str, loc), Expression.Lit(Literal.Str("world", loc), Type.Str, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Str("hello", loc), Type.Str, loc), rules, Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("world"))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Str02") {
//    // "hello" match { case "bonjour" => 1; case "hola" => 2; case _ => 0 }
//    val rules = List(
//      (Pattern.Lit(Literal.Str("bonjour", loc), Type.Str, loc), Expression.Lit(Literal.Int(1, loc), Type.Int32, loc)),
//      (Pattern.Lit(Literal.Str("hola", loc), Type.Str, loc), Expression.Lit(Literal.Int(2, loc), Type.Int32, loc)),
//      (Pattern.Wildcard(Type.Str, loc), Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Str("hello", loc), Type.Str, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tag01") {
//    // foo.bar.baz "hello world" match { case foo.bar.baz "hello world" => true }
//    val name = Name.Resolved.mk(List("foo", "bar"))
//    val ident = toIdent("baz")
//    val tagTpe = Type.Tag(name, ident, Type.Str)
//    val enumTpe = Type.Enum(Name.Resolved.mk("Family"), Map("foo.bar.baz" -> tagTpe))
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, ident, Literal.Str("hello world", loc), enumTpe, loc), tagTpe, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, ident, Literal.Str("hello world", loc), enumTpe, loc), tagTpe, loc),
//      rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tag02") {
//    // NameAndAge ("James", 42) match { case NameAndAge ("James", 40) => true; case _ => false }
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(Name.Resolved.mk("Family"), Map("Family.NameAndAge" -> tagTpe))
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, ident,
//        Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(40, loc)), Type.Tuple(List(Type.Str, Type.Int32)), loc),
//        enumTpe, loc), tagTpe, loc), Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      (Pattern.Wildcard(enumTpe, loc), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, ident,
//        Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(42, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc),
//      rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tag03") {
//    // ConstProp.Val 4 match { case ConstProp.Bot => true; case _ => false }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      (Pattern.Wildcard(enumTpe, loc), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc))
//    )
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identV, Literal.Int(4, loc), enumTpe, loc), tagTpeV, loc),
//      rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tag04") {
//    // ConstProp.Val 4 match { case ConstProp.Top => true; case _ => false }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      (Pattern.Wildcard(enumTpe, loc), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc))
//    )
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identV, Literal.Int(4, loc), enumTpe, loc), tagTpeV, loc),
//      rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tag05") {
//    // ConstProp.Val 4 match { case ConstProp.Val 4 => true; case _ => false }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identV, Literal.Int(4, loc), enumTpe, loc), tagTpeV, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      (Pattern.Wildcard(enumTpe, loc), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc))
//    )
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identV, Literal.Int(4, loc), enumTpe, loc), tagTpeV, loc),
//      rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tag06") {
//    // ConstProp.Val 4 match { case ConstProp.Val 5 => true; case _ => false }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identV, Literal.Int(5, loc), enumTpe, loc), tagTpeV, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      (Pattern.Wildcard(enumTpe, loc), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc))
//    )
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identV, Literal.Int(4, loc), enumTpe, loc), tagTpeV, loc),
//      rules, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tuple01") {
//    // ("hi", true) match { case ("hi", false) => 1; case _ => 2 }
//    val rules = List(
//      (Pattern.Lit(Literal.Tuple(List(Literal.Str("hi", loc), Literal.Bool(false, loc)),
//        Type.Tuple(List(Type.Str, Type.Bool)), loc),
//        Type.Tuple(List(Type.Str, Type.Bool)), loc), Expression.Lit(Literal.Int(1, loc), Type.Int32, loc)),
//      (Pattern.Wildcard(Type.Tuple(List(Type.Str, Type.Bool)), loc), Expression.Lit(Literal.Int(2, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tuple(List(Literal.Str("hi", loc), Literal.Bool(true, loc)),
//        Type.Tuple(List(Type.Str, Type.Bool)), loc), Type.Tuple(List(Type.Str, Type.Bool)), loc),
//      rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(2))(result)
//  }
//
//  test("Interpreter - Pattern.Literal.Tuple02") {
//    // (4, (12, 8)) match { case (4, (12, 8)) => 24 }
//    val rules = List((Pattern.Lit(
//      Literal.Tuple(List(
//        Literal.Int(4, loc),
//        Literal.Tuple(List(Literal.Int(12, loc), Literal.Int(8, loc)),
//          Type.Tuple(List(Type.Int32, Type.Int32)), loc)),
//        Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      Expression.Lit(Literal.Int(24, loc), Type.Int32, loc)))
//    val input = Expression.Match(Expression.Lit(
//      Literal.Tuple(List(
//        Literal.Int(4, loc),
//        Literal.Tuple(List(Literal.Int(12, loc), Literal.Int(8, loc)),
//          Type.Tuple(List(Type.Int32, Type.Int32)), loc)),
//        Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(24))(result)
//  }
//
//  test("Interpreter - Pattern.Tag01") {
//    // NameAndAge ("James", 42) match { case NameAndAge (_, age) => age }
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(name, Map("Family.NameAndAge" -> tagTpe))
//    val rules = List(
//      (Pattern.Tag(name, ident,
//        Pattern.Tuple(List(Pattern.Wildcard(Type.Str, loc), Pattern.Var(ident01, Type.Int32, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), tagTpe, loc), Expression.Var(ident01, Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, ident,
//        Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(42, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc),
//      rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(42))(result)
//  }
//
//  test("Interpreter - Pattern.Tag02") {
//    // NameAndAge ("James", 42) match { case NameAndAge ("James", age) => age; case NameAndAge _ => 0 }
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(name, Map("Family.NameAndAge" -> tagTpe))
//    val rules = List(
//      (Pattern.Tag(name, ident,
//        Pattern.Tuple(List(Pattern.Lit(Literal.Str("James", loc), Type.Str, loc), Pattern.Var(ident01, Type.Int32, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), tagTpe, loc), Expression.Var(ident01, Type.Int32, loc)),
//      (Pattern.Wildcard(Type.Str, loc), Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, ident,
//        Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(42, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc),
//      rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(42))(result)
//  }
//
//  test("Interpreter - Pattern.Tag03") {
//    // NameAndAge ("John", 42) match { case NameAndAge ("James", age) => age; case NameAndAge _ => 0 }
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(name, Map("Family.NameAndAge" -> tagTpe))
//    val rules = List(
//      (Pattern.Tag(name, ident,
//        Pattern.Tuple(List(Pattern.Lit(Literal.Str("James", loc), Type.Str, loc), Pattern.Var(ident01, Type.Int32, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), tagTpe, loc), Expression.Var(ident01, Type.Int32, loc)),
//      (Pattern.Wildcard(Type.Str, loc), Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, ident,
//        Literal.Tuple(List(Literal.Str("John", loc), Literal.Int(42, loc)),
//          Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc),
//      rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - Pattern.Tag04") {
//    // ConstProp.Top match {
//    //   case ConstProp.Top => 0
//    //   case ConstProp.Val v => v
//    //   case ConstProp.Bot => 0
//    // }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)),
//      (Pattern.Tag(name, identV, Pattern.Var(ident01, Type.Int32, loc), tagTpeV, loc), Expression.Var(ident01, Type.Int32, loc)),
//      (Pattern.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - Pattern.Tag05") {
//    // ConstProp.Bot match {
//    //   case ConstProp.Top => 0
//    //   case ConstProp.Val v => v
//    //   case ConstProp.Bot => 0
//    // }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)),
//      (Pattern.Tag(name, identV, Pattern.Var(ident01, Type.Int32, loc), tagTpeV, loc), Expression.Var(ident01, Type.Int32, loc)),
//      (Pattern.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - Pattern.Tag06") {
//    // ConstProp.Val 42 match {
//    //   case ConstProp.Top => 0
//    //   case ConstProp.Val v => v
//    //   case ConstProp.Bot => 0
//    // }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)),
//      (Pattern.Tag(name, identV, Pattern.Var(ident01, Type.Int32, loc), tagTpeV, loc), Expression.Var(ident01, Type.Int32, loc)),
//      (Pattern.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identV, Literal.Int(42, loc), enumTpe, loc), tagTpeV, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(42))(result)
//  }
//
//  test("Interpreter - Pattern.Tag07") {
//    // ConstProp.Val 100 match {
//    //   case ConstProp.Top => 0
//    //   case ConstProp.Val v => v
//    //   case ConstProp.Bot => 0
//    // }
//    import ConstantPropTagDefs._
//    val rules = List(
//      (Pattern.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)),
//      (Pattern.Tag(name, identV, Pattern.Var(ident01, Type.Int32, loc), tagTpeV, loc), Expression.Var(ident01, Type.Int32, loc)),
//      (Pattern.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc),
//        Expression.Lit(Literal.Int(0, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tag(name, identV, Literal.Int(100, loc), enumTpe, loc), tagTpeV, loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(100))(result)
//  }
//
//  test("Interpreter - Pattern.Tuple01") {
//    // (5, 6) match { case (x, y) => x + y }
//    val rules = List(
//      (Pattern.Tuple(List(Pattern.Var(ident01, Type.Int32, loc), Pattern.Var(ident02, Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Binary(BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc), Expression.Var(ident02, Type.Int32, loc), Type.Int32, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tuple(List(Literal.Int(5, loc), Literal.Int(6, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Type.Tuple(List(Type.Int32, Type.Int32)), loc), rules, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(11))(result)
//  }
//
//  test("Interpreter - Pattern.Tuple02") {
//    // (5, 6) match { case (5, 6) => "abc"; case (5, _) => "def"; case (_, _) => "ghi" }
//    val rules = List(
//      (Pattern.Tuple(List(Pattern.Lit(Literal.Int(5, loc), Type.Int32, loc), Pattern.Lit(Literal.Int(6, loc), Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("abc", loc), Type.Str, loc)),
//      (Pattern.Tuple(List(Pattern.Lit(Literal.Int(5, loc), Type.Int32, loc), Pattern.Wildcard(Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("def", loc), Type.Str, loc)),
//      (Pattern.Tuple(List(Pattern.Wildcard(Type.Int32, loc), Pattern.Wildcard(Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("ghi", loc), Type.Str, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tuple(List(Literal.Int(5, loc), Literal.Int(6, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Type.Tuple(List(Type.Int32, Type.Int32)), loc), rules, Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("abc"))(result)
//  }
//
//  test("Interpreter - Pattern.Tuple03") {
//    // (5, 16) match { case (5, 6) => "abc"; case (5, _) => "def"; case (_, _) => "ghi" }
//    val rules = List(
//      (Pattern.Tuple(List(Pattern.Lit(Literal.Int(5, loc), Type.Int32, loc), Pattern.Lit(Literal.Int(6, loc), Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("abc", loc), Type.Str, loc)),
//      (Pattern.Tuple(List(Pattern.Lit(Literal.Int(5, loc), Type.Int32, loc), Pattern.Wildcard(Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("def", loc), Type.Str, loc)),
//      (Pattern.Tuple(List(Pattern.Wildcard(Type.Int32, loc), Pattern.Wildcard(Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("ghi", loc), Type.Str, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tuple(List(Literal.Int(5, loc), Literal.Int(16, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Type.Tuple(List(Type.Int32, Type.Int32)), loc), rules, Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("def"))(result)
//  }
//
//  test("Interpreter - Pattern.Tuple04") {
//    // (15, 16) match { case (5, 6) => "abc"; case (5, _) => "def"; case (_, _) => "ghi" }
//    val rules = List(
//      (Pattern.Tuple(List(Pattern.Lit(Literal.Int(5, loc), Type.Int32, loc), Pattern.Lit(Literal.Int(6, loc), Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("abc", loc), Type.Str, loc)),
//      (Pattern.Tuple(List(Pattern.Lit(Literal.Int(5, loc), Type.Int32, loc), Pattern.Wildcard(Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("def", loc), Type.Str, loc)),
//      (Pattern.Tuple(List(Pattern.Wildcard(Type.Int32, loc), Pattern.Wildcard(Type.Int32, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Expression.Lit(Literal.Str("ghi", loc), Type.Str, loc)))
//    val input = Expression.Match(
//      Expression.Lit(Literal.Tuple(List(Literal.Int(15, loc), Literal.Int(16, loc)),
//        Type.Tuple(List(Type.Int32, Type.Int32)), loc), Type.Tuple(List(Type.Int32, Type.Int32)), loc), rules, Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("ghi"))(result)
//  }
//
//  test("Interpreter - Scala.Tuple01") {
//    import ca.uwaterloo.flix.api.InvokableUnsafe
//
//    val s =
//      """
//        |rel A(x: Native, y: Native);
//        |rel B(x: Native);
//        |
//        |def fst(t: (Native, Native)): Native = match t with {
//        |  case (x, w) => x
//        |}
//        |def snd(t: (Native, Native)): Native = match t with {
//        |  case (w, y) => y
//        |}
//        |
//        |B(f()).
//        |A(fst(t), snd(t)) :- B(t).
//      """.stripMargin
//
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(), flix.mkNativeType)
//    flix
//      .addStr(s)
//      .addHookUnsafe("f", tpe, new InvokableUnsafe {
//        override def apply(args: Array[AnyRef]): AnyRef = ("abc", 22)
//      })
//
//    val model = flix.solve().get
//    val A = model.relations(Name.Resolved.mk(List("A"))).toSet
//    assert(A.contains(List("abc", new java.lang.Integer(22))))
//  }
//
//  test("Interpreter - Expression.Match.Error01") {
//    // 123 match { case 321 => Unit }
//    val rules = List((Pattern.Lit(Literal.Int(321, loc), Type.Int32, loc), Expression.Lit(Literal.Unit(loc), Type.Unit, loc)))
//    val input = Expression.Match(Expression.Lit(Literal.Int(123, loc), Type.Int32, loc), rules, Type.Int32, loc)
//    intercept[RuntimeException] {
//      Interpreter.eval(input, root)
//    }
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Tuples, Tags, and Sets                                    //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.Tuple01") {
//    val input = Expression.Tuple(List(
//      Expression.Lit(Literal.Int(42, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Str("hi", loc), Type.Str, loc)),
//      Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.Tuple(Array(Value.mkInt32(42), Value.False, Value.mkStr("hi"))))(result)
//  }
//
//  test("Interpreter - Expression.Tuple02") {
//    val input = Expression.Tuple(List(
//      Expression.Lit(Literal.Int(4, loc), Type.Int32, loc),
//      Expression.Tuple(List(Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(8, loc), Type.Int32, loc)), Type.Tuple(List(Type.Int32, Type.Int32)), loc)),
//      Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.Tuple(Array(Value.mkInt32(4), Value.Tuple(Array(Value.mkInt32(12), Value.mkInt32(8))))))(result)
//  }
//
//  test("Interpreter - Expression.Tuple03") {
//    val input = Expression.Tuple(List(
//      // 40 + 2
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Lit(Literal.Int(40, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      // !(-12 < 22)
//      Expression.Unary(
//        UnaryOperator.LogicalNot,
//        Expression.Binary(
//          BinaryOperator.Less,
//          Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//          Expression.Lit(Literal.Int(22, loc), Type.Int32, loc),
//          Type.Bool, loc),
//        Type.Bool, loc),
//      // if (true) "hi" else "hello"
//      Expression.IfThenElse(
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//        Expression.Lit(Literal.Str("hi", loc), Type.Str, loc),
//        Expression.Lit(Literal.Str("hello", loc), Type.Str, loc),
//        Type.Str, loc)),
//      Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.Tuple(Array(Value.mkInt32(42), Value.False, Value.mkStr("hi"))))(result)
//  }
//
//  test("Interpreter - Expression.Set01") {
//    val input = "fn f: Set[Int] = #{1, 4, 2}"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkSet(Set(Value.mkInt32(1), Value.mkInt32(4), Value.mkInt32(2))))(result)
//  }
//
//  test("Interpreter - Expression.Set02") {
//    val input = "fn f: Set[Int] = #{1 + 2, 3 * 4, 5 - 6}"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkSet(Set(Value.mkInt32(-1), Value.mkInt32(12), Value.mkInt32(3))))(result)
//  }
//
//  test("Interpreter - Expression.Set03") {
//    val input = "fn f: Set[(Int, Bool)] = #{(1 + 2, true), (2 + 1, !false), (4 * 7, true), (5, true && false)}"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkSet(Set(
//      Value.Tuple(Array(Value.mkInt32(3), Value.True)),
//      Value.Tuple(Array(Value.mkInt32(28), Value.True)),
//      Value.Tuple(Array(Value.mkInt32(5), Value.False))
//    )))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Error                                                     //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.Error01") {
//    val input = Expression.Error(Type.Unit, loc)
//    intercept[RuntimeException] {
//      Interpreter.eval(input, root)
//    }
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // evalHeadTerm - Var                                                      //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("evalHeadTerm - Var01") {
//    val input = Term.Head.Lit(Literal.Str("hello", loc), Type.Str, loc)
//    val env = mutable.Map.empty[String, AnyRef] + (ident01.name -> Value.False)
//    val result = Interpreter.evalHeadTerm(input, root, env)
//    assertResult(Value.mkStr("hello"))(result)
//  }
//
//  test("evalHeadTerm - Var02") {
//    val input = Term.Head.Var(ident01, Type.Int32, loc)
//    val env = mutable.Map.empty[String, AnyRef] + (ident01.name -> Value.mkInt32(5))
//    val result = Interpreter.evalHeadTerm(input, root, env)
//    assertResult(Value.mkInt32(5))(result)
//  }
//
//  test("evalHeadTerm - Var03") {
//    val input = Term.Head.Var(ident01, Type.Bool, loc)
//    val env = mutable.Map.empty[String, AnyRef] + (ident01.name -> Value.False)
//    val result = Interpreter.evalHeadTerm(input, root, env)
//    assertResult(Value.False)(result)
//  }
//
//  test("evalHeadTerm - Var04") {
//    val input = Term.Head.Var(ident02, Type.Str, loc)
//    val env = mutable.Map.empty[String, AnyRef] +(ident01.name -> Value.mkStr("foo"), ident02.name -> Value.mkStr("bar"))
//    val result = Interpreter.evalHeadTerm(input, root, env)
//    assertResult(Value.mkStr("bar"))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // evalHeadTerm - Literals                                                 //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("evalHeadTerm - Literal.Unit") {
//    val input = Term.Head.Lit(Literal.Unit(loc), Type.Unit, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.Unit)(result)
//  }
//
//  test("evalHeadTerm - Literal.Bool01") {
//    val input = Term.Head.Lit(Literal.Bool(true, loc), Type.Bool, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.True)(result)
//  }
//
//  test("evalHeadTerm - Literal.Bool02") {
//    val input = Term.Head.Lit(Literal.Bool(false, loc), Type.Bool, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.False)(result)
//  }
//
//  test("evalHeadTerm - Literal.Int01") {
//    val input = Term.Head.Lit(Literal.Int(-242, loc), Type.Int32, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(-242))(result)
//  }
//
//  test("evalHeadTerm - Literal.Int02") {
//    val input = Term.Head.Lit(Literal.Int(-42, loc), Type.Int32, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(-42))(result)
//  }
//
//  test("evalHeadTerm - Literal.Int03") {
//    val input = Term.Head.Lit(Literal.Int(0, loc), Type.Int32, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("evalHeadTerm - Literal.Int04") {
//    val input = Term.Head.Lit(Literal.Int(98, loc), Type.Int32, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(98))(result)
//  }
//
//  test("evalHeadTerm - Literal.Int05") {
//    val input = Term.Head.Lit(Literal.Int(91238, loc), Type.Int32, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(91238))(result)
//  }
//
//  test("evalHeadTerm - Literal.Str01") {
//    val input = Term.Head.Lit(Literal.Str("", loc), Type.Str, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkStr(""))(result)
//  }
//
//  test("evalHeadTerm - Literal.Str02") {
//    val input = Term.Head.Lit(Literal.Str("Hello World!", loc), Type.Str, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkStr("Hello World!"))(result)
//  }
//
//  test("evalHeadTerm - Literal.Str03") {
//    val input = Term.Head.Lit(Literal.Str("asdf", loc), Type.Str, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkStr("asdf"))(result)
//  }
//
//  test("evalHeadTerm - Literal.Str04") {
//    val input = Term.Head.Lit(Literal.Str("foobar", loc), Type.Str, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkStr("foobar"))(result)
//  }
//
//  test("evalHeadTerm - Literal.Str05") {
//    val input = Term.Head.Lit(Literal.Str("\"\"\"", loc), Type.Str, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkStr("\"\"\""))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tuple01") {
//    val input = Term.Head.Lit(
//      Literal.Tuple(List(Literal.Int(42, loc), Literal.Bool(false, loc), Literal.Str("hi", loc)),
//        Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc),
//      Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.Tuple(Array(Value.mkInt32(42), Value.False, Value.mkStr("hi"))))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tuple02") {
//    val input = Term.Head.Lit(
//      Literal.Tuple(List(
//        Literal.Int(4, loc),
//        Literal.Tuple(List(Literal.Int(12, loc), Literal.Int(8, loc)),
//          Type.Tuple(List(Type.Int32, Type.Int32)), loc)),
//        Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.Tuple(Array(Value.mkInt32(4), Value.Tuple(Array(Value.mkInt32(12), Value.mkInt32(8))))))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tag01") {
//    val name = Name.Resolved.mk(List("foo", "bar"))
//    val ident = toIdent("baz")
//    val tagTpe = Type.Tag(name, ident, Type.Str)
//    val enumTpe = Type.Enum(Name.Resolved.mk("foo"), Map("foo.bar.baz" -> tagTpe))
//    val input = Term.Head.Lit(Literal.Tag(name, ident, Literal.Str("hello world", loc), enumTpe, loc), tagTpe, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "baz", Value.mkStr("hello world")))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tag02") {
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(Name.Resolved.mk("ConstProp"), Map("Family.NameAndAge" -> tagTpe))
//    val input = Term.Head.Lit(Literal.Tag(name, ident,
//      Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(42, loc)),
//        Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "NameAndAge", Value.Tuple(Array(Value.mkStr("James"), Value.mkInt32(42)))))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tag03") {
//    import ConstantPropTagDefs._
//    val input = Term.Head.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "Bot", Value.Unit))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tag04") {
//    import ConstantPropTagDefs._
//    val input = Term.Head.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "Top", Value.Unit))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tag05") {
//    import ConstantPropTagDefs._
//    val input = Term.Head.Lit(Literal.Tag(name, identV, Literal.Int(0, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(0)))(result)
//
//  }
//
//  test("evalHeadTerm - Literal.Tag06") {
//    import ConstantPropTagDefs._
//    val input = Term.Head.Lit(Literal.Tag(name, identV, Literal.Int(-240, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(-240)))(result)
//  }
//
//  test("evalHeadTerm - Literal.Tag07") {
//    import ConstantPropTagDefs._
//    val input = Term.Head.Lit(Literal.Tag(name, identV, Literal.Int(1241, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.evalHeadTerm(input, root, mutable.Map.empty)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(1241)))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // evalHeadTerm - Apply                                                    //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("evalHeadTerm - Apply01") {
//    // def foo.bar = () => false
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc), Type.Lambda(List(), Type.Bool), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar()
//    val apply = Term.Head.Apply(name01, List(), Type.Bool, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.False)(value)
//  }
//
//  test("evalHeadTerm - Apply02") {
//    // def foo.bar = x => 3
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)), Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(4)
//    val apply = Term.Head.Apply(name01, List(Term.Head.Lit(Literal.Int(4, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(3))(value)
//  }
//
//  test("evalHeadTerm - Apply03") {
//    // def foo.bar = x => x
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)), Expression.Var(ident01, Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(5)
//    val apply = Term.Head.Apply(name01, List(Term.Head.Lit(Literal.Int(5, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(5))(value)
//  }
//
//  test("evalHeadTerm - Apply04") {
//    // def foo.bar = x => 1 + 2
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Lit(Literal.Int(1, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(42)
//    val apply = Term.Head.Apply(name01, List(Term.Head.Lit(Literal.Int(42, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(3))(value)
//  }
//
//  test("evalHeadTerm - Apply05") {
//    // def foo.bar = x => x + 2
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(100)
//    val apply = Term.Head.Apply(name01, List(Term.Head.Lit(Literal.Int(100, loc), Type.Int32, loc)), Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(102))(value)
//  }
//
//  test("evalHeadTerm - Apply06") {
//    // def foo.bar = (x, y) => x + y
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32), FormalArg(ident02, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Var(ident02, Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32, Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(3, 4)
//    val apply = Term.Head.Apply(name01, List(
//      Term.Head.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Term.Head.Lit(Literal.Int(4, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(7))(value)
//  }
//
//  test("evalHeadTerm - Apply07") {
//    // def foo.bar = (x, y) => if (x) then true else y
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Bool), FormalArg(ident02, Type.Bool)),
//      Expression.IfThenElse(
//        Expression.Var(ident01, Type.Bool, loc),
//        Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//        Expression.Var(ident02, Type.Bool, loc),
//        Type.Bool, loc),
//      Type.Lambda(List(Type.Bool, Type.Bool), Type.Bool), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(false, true)
//    val apply = Term.Head.Apply(name01, List(
//      Term.Head.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Term.Head.Lit(Literal.Bool(true, loc), Type.Bool, loc)),
//      Type.Bool, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.True)(value)
//  }
//
//  test("evalHeadTerm - Apply08") {
//    // def foo.bar = (x, y, z) => x + (y + z)
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32), FormalArg(ident02, Type.Int32), FormalArg(ident03, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident02, Type.Int32, loc),
//          Expression.Var(ident03, Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32, Type.Int32, Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(2, 42, 5)
//    val apply = Term.Head.Apply(name01, List(
//      Term.Head.Lit(Literal.Int(2, loc), Type.Int32, loc),
//      Term.Head.Lit(Literal.Int(42, loc), Type.Int32, loc),
//      Term.Head.Lit(Literal.Int(5, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(49))(value)
//  }
//
//  test("evalHeadTerm - Apply09") {
//    // def foo.bar = x => (y => x + y)
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Lambda(
//        Ast.Annotations(List.empty),
//        List(FormalArg(ident02, Type.Int32)),
//        Expression.Binary(
//          BinaryOperator.Plus,
//          Expression.Var(ident01, Type.Int32, loc),
//          Expression.Var(ident02, Type.Int32, loc),
//          Type.Int32, loc),
//        Type.Lambda(List(Type.Int32), Type.Int32), loc),
//      Type.Lambda(List(Type.Int32), Type.Lambda(List(Type.Int32), Type.Int32)), loc)
//    val definition = Definition.Constant(name01,
//      Expression.Apply(lambda, List(Expression.Lit(Literal.Int(3, loc), Type.Int32, loc)),
//        Type.Lambda(List(Type.Int32), Type.Int32), loc),
//      Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // foo.bar(3)(4)
//    val apply = Term.Head.Apply(
//      name01,
//      List(Term.Head.Lit(Literal.Int(4, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.mkInt32(7))(value)
//  }
//
//  test("evalHeadTerm - Apply10") {
//    // def foo.bar = x, y => x(y)
//    val lambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Lambda(List(Type.Int32), Type.Int32)), FormalArg(ident02, Type.Int32)),
//      Expression.Apply(
//        Expression.Var(ident01, Type.Lambda(List(Type.Int32), Type.Int32), loc),
//        List(Expression.Var(ident02, Type.Int32, loc)),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Lambda(List(Type.Int32), Type.Int32), Type.Int32), Type.Int32), loc)
//    val definition = Definition.Constant(name01, lambda, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//    val innerLambda = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(FormalArg(ident01, Type.Int32)),
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Var(ident01, Type.Int32, loc),
//        Expression.Lit(Literal.Int(1, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      Type.Lambda(List(Type.Int32), Type.Int32), loc)
//    val closure = Interpreter.eval(innerLambda, root, mutable.Map())
//
//    // foo.bar((x => x + 1), 5)
//    val apply = Term.Head.Apply(name01, List(Term.Head.Var(ident03, Type.Lambda(List(Type.Int32), Type.Int32), loc),
//      Term.Head.Lit(Literal.Int(5, loc), Type.Int32, loc)),
//      Type.Int32, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty[String, AnyRef] + (ident03.name -> closure))
//    assertResult(Value.mkInt32(6))(value)
//  }
//
//  test("evalHeadTerm - Apply11") {
//    // def foo.bar = () => false
//    // def abc.def = () => true
//    val lambda01 = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(), Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc), Type.Lambda(List(), Type.Bool), loc)
//    val lambda02 = Expression.Lambda(
//      Ast.Annotations(List.empty),
//      List(), Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc), Type.Lambda(List(), Type.Bool), loc)
//    val definition01 = Definition.Constant(name01, lambda01, Type.Lambda(List(), Type.Bool), loc)
//    val definition02 = Definition.Constant(name02, lambda02, Type.Lambda(List(), Type.Bool), loc)
//    val root = Root(Map(name01 -> definition01, name02 -> definition02),
//      Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//
//    // abc.def()
//    val apply = Term.Head.Apply(name02, List(), Type.Bool, loc)
//    val value = Interpreter.evalHeadTerm(apply, root, mutable.Map.empty)
//    assertResult(Value.True)(value)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Hook and Apply                                            //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("evalHeadTerm - HookApply01") {
//    val input =
//      s"""namespace A {
//          |  fn f(x: Int): Bool = g(x)
//          |  rel S(x: Int)
//          |  rel T(x: Bool)
//          |  S(42).
//          |  T(f(x)) :- S(x).
//          |};
//       """.stripMargin
//    var counter = 0
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
//    flix
//      .addStr(input)
//      .addHook("A::g", tpe, new Invokable {
//        def apply(args: Array[IValue]): IValue = {
//          counter += 1
//          flix.mkTrue
//        }
//      })
//    val model = flix.solve()
//    val result = model.get.relations(Name.Resolved.mk("A::T")).toSet
//    assertResult(1)(counter)
//    assertResult(Set(List(Value.True)))(result)
//  }
//
//  test("evalHeadTerm - HookApply02") {
//    val input =
//      s"""namespace A {
//          |  fn f(x: Int, y: Int): Int = g(x, y)
//          |  rel S(x: Int, y: Int)
//          |  rel T(x: Int)
//          |  S(40, 2).
//          |  S(2, 40).
//          |  S(1, 5).
//          |  T(f(x, y)) :- S(x, y).
//          |};
//       """.stripMargin
//    var counter = 0
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkInt32Type), flix.mkInt32Type)
//    flix
//      .addStr(input)
//      .addHook("A::g", tpe, new Invokable {
//        def apply(args: Array[IValue]): IValue = {
//          counter += 1
//          flix.mkInt32(args(0).getInt32 + args(1).getInt32)
//        }
//      })
//    val model = flix.solve()
//    val result = model.get.relations(Name.Resolved.mk("A::T")).toSet
//    assertResult(3)(counter)
//    assertResult(Set(List(Value.mkInt32(42)), List(Value.mkInt32(6))))(result)
//  }
//
//  test("evalHeadTerm - HookApply03") {
//    val input =
//      s"""namespace A {
//          |  fn f(x: Int): Int = g()
//          |  rel T(x: Int)
//          |  T(f(42)).
//          |};
//       """.stripMargin
//    var counter = 0
//    val flix = new Flix()
//    val tpe = flix.mkFunctionType(Array(), flix.mkInt32Type)
//    flix
//      .addStr(input)
//      .addHook("A::g", tpe, new Invokable {
//        def apply(args: Array[IValue]): IValue = {
//          counter += 1
//          flix.mkInt32(123)
//        }
//      })
//    val model = flix.solve()
//    val result = model.get.relations(Name.Resolved.mk("A::T")).toSet
//    assertResult(1)(counter)
//    assertResult(Set(List(Value.mkInt32(123))))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // evalBodyTerm - Var                                                      //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("evalBodyTerm - Var01") {
//    val input = Term.Body.Lit(Literal.Str("hello", loc), Type.Str, loc)
//    val env = mutable.Map(ident01.name -> Value.False)
//    val result = Interpreter.evalBodyTerm(input, env)
//    assertResult(Value.mkStr("hello"))(result)
//  }
//
//  test("evalBodyTerm - Var02") {
//    val input = Term.Body.Var(ident01, Type.Int32, loc)
//    val env = mutable.Map(ident01.name -> Value.mkInt32(5))
//    val result = Interpreter.evalBodyTerm(input, env)
//    assertResult(Value.mkInt32(5))(result)
//  }
//
//  test("evalBodyTerm - Var03") {
//    val input = Term.Body.Var(ident01, Type.Bool, loc)
//    val env = mutable.Map(ident01.name -> Value.False)
//    val result = Interpreter.evalBodyTerm(input, env)
//    assertResult(Value.False)(result)
//  }
//
//  test("evalBodyTerm - Var04") {
//    val input = Term.Body.Var(ident02, Type.Str, loc)
//    val env = mutable.Map(ident01.name -> Value.mkStr("foo"), ident02.name -> Value.mkStr("bar"))
//    val result = Interpreter.evalBodyTerm(input, env)
//    assertResult(Value.mkStr("bar"))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // evalBodyTerm - Literals                                                 //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("evalBodyTerm - Literal.Unit") {
//    val input = Term.Body.Lit(Literal.Unit(loc), Type.Unit, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.Unit)(result)
//  }
//
//  test("evalBodyTerm - Literal.Bool01") {
//    val input = Term.Body.Lit(Literal.Bool(true, loc), Type.Bool, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.True)(result)
//  }
//
//  test("evalBodyTerm - Literal.Bool02") {
//    val input = Term.Body.Lit(Literal.Bool(false, loc), Type.Bool, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.False)(result)
//  }
//
//  test("evalBodyTerm - Literal.Int01") {
//    val input = Term.Body.Lit(Literal.Int(-242, loc), Type.Int32, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkInt32(-242))(result)
//  }
//
//  test("evalBodyTerm - Literal.Int02") {
//    val input = Term.Body.Lit(Literal.Int(-42, loc), Type.Int32, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkInt32(-42))(result)
//  }
//
//  test("evalBodyTerm - Literal.Int03") {
//    val input = Term.Body.Lit(Literal.Int(0, loc), Type.Int32, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("evalBodyTerm - Literal.Int04") {
//    val input = Term.Body.Lit(Literal.Int(98, loc), Type.Int32, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkInt32(98))(result)
//  }
//
//  test("evalBodyTerm - Literal.Int05") {
//    val input = Term.Body.Lit(Literal.Int(91238, loc), Type.Int32, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkInt32(91238))(result)
//  }
//
//  test("evalBodyTerm - Literal.Str01") {
//    val input = Term.Body.Lit(Literal.Str("", loc), Type.Str, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkStr(""))(result)
//  }
//
//  test("evalBodyTerm - Literal.Str02") {
//    val input = Term.Body.Lit(Literal.Str("Hello World!", loc), Type.Str, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkStr("Hello World!"))(result)
//  }
//
//  test("evalBodyTerm - Literal.Str03") {
//    val input = Term.Body.Lit(Literal.Str("asdf", loc), Type.Str, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkStr("asdf"))(result)
//  }
//
//  test("evalBodyTerm - Literal.Str04") {
//    val input = Term.Body.Lit(Literal.Str("foobar", loc), Type.Str, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkStr("foobar"))(result)
//  }
//
//  test("evalBodyTerm - Literal.Str05") {
//    val input = Term.Body.Lit(Literal.Str("\"\"\"", loc), Type.Str, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkStr("\"\"\""))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tuple01") {
//    val input = Term.Body.Lit(
//      Literal.Tuple(List(Literal.Int(42, loc), Literal.Bool(false, loc), Literal.Str("hi", loc)),
//        Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc),
//      Type.Tuple(List(Type.Int32, Type.Bool, Type.Str)), loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.Tuple(Array(Value.mkInt32(42), Value.False, Value.mkStr("hi"))))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tuple02") {
//    val input = Term.Body.Lit(
//      Literal.Tuple(List(
//        Literal.Int(4, loc),
//        Literal.Tuple(List(Literal.Int(12, loc), Literal.Int(8, loc)),
//          Type.Tuple(List(Type.Int32, Type.Int32)), loc)),
//        Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc),
//      Type.Tuple(List(Type.Int32, Type.Tuple(List(Type.Int32, Type.Int32)))), loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.Tuple(Array(Value.mkInt32(4), Value.Tuple(Array(Value.mkInt32(12), Value.mkInt32(8))))))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tag01") {
//    val name = Name.Resolved.mk(List("foo", "bar"))
//    val ident = toIdent("baz")
//    val tagTpe = Type.Tag(name, ident, Type.Str)
//    val enumTpe = Type.Enum(Name.Resolved.mk("foo"), Map("foo.bar.baz" -> tagTpe))
//    val input = Term.Body.Lit(Literal.Tag(name, ident, Literal.Str("hello world", loc), enumTpe, loc), tagTpe, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "baz", Value.mkStr("hello world")))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tag02") {
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(Name.Resolved.mk("Family"), Map("Family.NameAndAge" -> tagTpe))
//    val input = Term.Body.Lit(Literal.Tag(name, ident,
//      Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(42, loc)),
//        Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "NameAndAge", Value.Tuple(Array(Value.mkStr("James"), Value.mkInt32(42)))))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tag03") {
//    import ConstantPropTagDefs._
//    val input = Term.Body.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "Bot", Value.Unit))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tag04") {
//    import ConstantPropTagDefs._
//    val input = Term.Body.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "Top", Value.Unit))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tag05") {
//    import ConstantPropTagDefs._
//    val input = Term.Body.Lit(Literal.Tag(name, identV, Literal.Int(0, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(0)))(result)
//
//  }
//
//  test("evalBodyTerm - Literal.Tag06") {
//    import ConstantPropTagDefs._
//    val input = Term.Body.Lit(Literal.Tag(name, identV, Literal.Int(-240, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(-240)))(result)
//  }
//
//  test("evalBodyTerm - Literal.Tag07") {
//    import ConstantPropTagDefs._
//    val input = Term.Body.Lit(Literal.Tag(name, identV, Literal.Int(1241, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.evalBodyTerm(input, mutable.Map())
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(1241)))(result)
//  }

}
