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

  // TODO: Merge Literal.{Tuple,Tag,Set} tests with Expression.{Tuple,Tag,Set} tests

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
//  test("Interpreter - Literal.Tag01") {
//    val name = Name.Resolved.mk(List("foo", "bar"))
//    val ident = toIdent("baz")
//    val tagTpe = Type.Tag(name, ident, Type.Str)
//    val enumTpe = Type.Enum(Name.Resolved.mk("ConstProp"), Map("foo.bar.baz" -> tagTpe))
//    val input = Expression.Lit(Literal.Tag(name, ident, Literal.Str("hello world", loc), enumTpe, loc), tagTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "baz", Value.mkStr("hello world")))(result)
//  }
//
//  test("Interpreter - Literal.Tag02") {
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(Name.Resolved.mk("Family"), Map("Family.NameAndAge" -> tagTpe))
//    val input = Expression.Lit(Literal.Tag(name, ident,
//      Literal.Tuple(List(Literal.Str("James", loc), Literal.Int(42, loc)),
//        Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc), tagTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "NameAndAge", Value.Tuple(Array(Value.mkStr("James"), Value.mkInt32(42)))))(result)
//  }
//
//  test("Interpreter - Literal.Tag03") {
//    import ConstantPropTagDefs._
//    val input = Expression.Lit(Literal.Tag(name, identB, Literal.Unit(loc), enumTpe, loc), tagTpeB, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Bot", Value.Unit))(result)
//  }
//
//  test("Interpreter - Literal.Tag04") {
//    import ConstantPropTagDefs._
//    val input = Expression.Lit(Literal.Tag(name, identT, Literal.Unit(loc), enumTpe, loc), tagTpeT, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Top", Value.Unit))(result)
//  }
//
//  test("Interpreter - Literal.Tag05") {
//    import ConstantPropTagDefs._
//    val input = Expression.Lit(Literal.Tag(name, identV, Literal.Int(0, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(0)))(result)
//
//  }
//
//  test("Interpreter - Literal.Tag06") {
//    import ConstantPropTagDefs._
//    val input = Expression.Lit(Literal.Tag(name, identV, Literal.Int(-240, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(-240)))(result)
//  }
//
//  test("Interpreter - Literal.Tag07") {
//    import ConstantPropTagDefs._
//    val input = Expression.Lit(Literal.Tag(name, identV, Literal.Int(1241, loc), enumTpe, loc), tagTpeV, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(1241)))(result)
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

//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Var                                                       //
//  /////////////////////////////////////////////////////////////////////////////
//
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
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Ref                                                       //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.Ref01") {
//    val name = Name.Resolved.mk(List("foo", "bar", "baz"))
//    val const = Definition.Constant(name, Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc), Type.Bool, loc)
//    val root = Root(Map(name -> const), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//    val input = Expression.Lit(Literal.Str("hello", loc), Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("hello"))(result)
//  }
//
//  test("Interpreter - Expression.Ref02") {
//    val name = Name.Resolved.mk(List("foo", "bar", "baz"))
//    val const = Definition.Constant(name, Expression.Lit(Literal.Int(5, loc), Type.Int32, loc), Type.Int32, loc)
//    val root = Root(Map(name -> const), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//    val input = Expression.Ref(name, Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(5))(result)
//  }
//
//  test("Interpreter - Expression.Ref03") {
//    val name = Name.Resolved.mk(List("foo", "bar", "baz"))
//    val const = Definition.Constant(name, Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc), Type.Bool, loc)
//    val root = Root(Map(name -> const), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//    val input = Expression.Ref(name, Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - Expression.Ref04") {
//    val name01 = Name.Resolved.mk(List("foo", "bar", "baz"))
//    val name02 = Name.Resolved.mk(List("abc", "def", "ghi"))
//    val const01 = Definition.Constant(name01, Expression.Lit(Literal.Str("foo", loc), Type.Str, loc), Type.Str, loc)
//    val const02 = Definition.Constant(name01, Expression.Lit(Literal.Str("bar", loc), Type.Str, loc), Type.Str, loc)
//    val root = Root(Map(name01 -> const01, name02 -> const02), Map(), Map(), Map(), List(), List(), Map.empty, new Time(0, 0, 0, 0, 0))
//    val input = Expression.Ref(name02, Type.Str, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("bar"))(result)
//  }
//
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
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Unary                                                     //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - UnaryOperator.Not01") {
//    val input = Expression.Unary(
//      UnaryOperator.LogicalNot,
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - UnaryOperator.Not02") {
//    val input = Expression.Unary(
//      UnaryOperator.LogicalNot,
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Type.Bool, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - UnaryOperator.UnaryPlus01") {
//    val input = Expression.Unary(
//      UnaryOperator.Plus,
//      Expression.Lit(Literal.Int(23, loc), Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(23))(result)
//  }
//
//  test("Interpreter - UnaryOperator.UnaryPlus02") {
//    val input = Expression.Unary(
//      UnaryOperator.Plus,
//      Expression.Lit(Literal.Int(-4, loc), Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-4))(result)
//  }
//
//  test("Interpreter - UnaryOperator.UnaryMinus01") {
//    val input = Expression.Unary(
//      UnaryOperator.Minus,
//      Expression.Lit(Literal.Int(23, loc), Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-23))(result)
//  }
//
//  test("Interpreter - UnaryOperator.UnaryMinus02") {
//    val input = Expression.Unary(
//      UnaryOperator.Minus,
//      Expression.Lit(Literal.Int(-4, loc), Type.Int32, loc),
//      Type.Int32, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(4))(result)
//  }
//
//  test("Interpreter - UnaryOperator.UnaryNegate01") {
//    val input = "fn f: Int = ~42"
//    val model =  new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(~42))(result)
//  }
//
//  test("Interpreter - UnaryOperator.UnaryNegate02") {
//    val input = "fn f: Int = ~~42"
//    val model =  new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42))(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - Binary                                                    //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - BinaryOperator.Plus01") {
//    val input = Expression.Binary(
//      BinaryOperator.Plus,
//      Expression.Lit(Literal.Int(400, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(100, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(500))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Plus02") {
//    val input = Expression.Binary(
//      BinaryOperator.Plus,
//      Expression.Lit(Literal.Int(100, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(400, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(500))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Plus03") {
//    val input = Expression.Binary(
//      BinaryOperator.Plus,
//      Expression.Lit(Literal.Int(-400, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(100, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-300))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Plus04") {
//    val input = Expression.Binary(
//      BinaryOperator.Plus,
//      Expression.Lit(Literal.Int(-100, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(400, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(300))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Plus05") {
//    val input = Expression.Binary(
//      BinaryOperator.Plus,
//      Expression.Lit(Literal.Int(-400, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-100, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-500))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Minus01") {
//    val input = Expression.Binary(
//      BinaryOperator.Minus,
//      Expression.Lit(Literal.Int(400, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(100, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(300))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Minus02") {
//    val input = Expression.Binary(
//      BinaryOperator.Minus,
//      Expression.Lit(Literal.Int(100, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(400, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-300))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Minus03") {
//    val input = Expression.Binary(
//      BinaryOperator.Minus,
//      Expression.Lit(Literal.Int(-400, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(100, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-500))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Minus04") {
//    val input = Expression.Binary(
//      BinaryOperator.Minus,
//      Expression.Lit(Literal.Int(-100, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(400, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-500))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Minus05") {
//    val input = Expression.Binary(
//      BinaryOperator.Minus,
//      Expression.Lit(Literal.Int(-400, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-100, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-300))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Times01") {
//    val input = Expression.Binary(
//      BinaryOperator.Times,
//      Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(6))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Times02") {
//    val input = Expression.Binary(
//      BinaryOperator.Times,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(6))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Times03") {
//    val input = Expression.Binary(
//      BinaryOperator.Times,
//      Expression.Lit(Literal.Int(-2, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-6))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Times04") {
//    val input = Expression.Binary(
//      BinaryOperator.Times,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-6))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Times05") {
//    val input = Expression.Binary(
//      BinaryOperator.Times,
//      Expression.Lit(Literal.Int(-2, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(6))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Divide01") {
//    val input = Expression.Binary(
//      BinaryOperator.Divide,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(4))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Divide02") {
//    val input = Expression.Binary(
//      BinaryOperator.Divide,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Divide03") {
//    val input = Expression.Binary(
//      BinaryOperator.Divide,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-4))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Divide04") {
//    val input = Expression.Binary(
//      BinaryOperator.Divide,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Divide05") {
//    val input = Expression.Binary(
//      BinaryOperator.Divide,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(4))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Modulo01") {
//    val input = Expression.Binary(
//      BinaryOperator.Modulo,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(2, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Modulo02") {
//    val input = Expression.Binary(
//      BinaryOperator.Modulo,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(5, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(2))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Modulo03") {
//    val input = Expression.Binary(
//      BinaryOperator.Modulo,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(5, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-2))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Modulo04") {
//    val input = Expression.Binary(
//      BinaryOperator.Modulo,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-5, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(2))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Modulo05") {
//    val input = Expression.Binary(
//      BinaryOperator.Modulo,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-5, loc), Type.Int32, loc),
//      Type.Int32, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkInt32(-2))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseAnd01") {
//    val input = "fn f: Int = 42 & 65535"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 & 65535))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseAnd02") {
//    val input = "fn f: Int = 42 & 0"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 & 0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseAnd03") {
//    val input = "fn f: Int = 42 & 42"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 & 42))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseOr01") {
//    val input = "fn f: Int = 42 | 65535"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 | 65535))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseOr02") {
//    val input = "fn f: Int = 42 | 0"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 | 0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseOr03") {
//    val input = "fn f: Int = 42 | 42"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 | 42))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseXor01") {
//    val input = "fn f: Int = 42 ^ 65535"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 ^ 65535))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseXor02") {
//    val input = "fn f: Int = 42 ^ 0"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 ^ 0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseXor03") {
//    val input = "fn f: Int = 42 ^ 42"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(42 ^ 42))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseLeftShift01") {
//    val input = "fn f: Int = 4 << 0"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(4 << 0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseLeftShift02") {
//    val input = "fn f: Int = 4 << 14"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(4 << 14))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseLeftShift03") {
//    val input = "fn f: Int = 4 << 29"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(4 << 29))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseLeftShift04") {
//    val input = "fn f: Int = 4 << 30"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(4 << 30))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseRightShift01") {
//    val input = "fn f: Int = 12345 >> 20"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(12345 >> 20))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseRightShift02") {
//    val input = "fn f: Int = 12345 >> 10"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(12345 >> 10))(result)
//  }
//
//  test("Interpreter - BinaryOperator.BitwiseRightShift03") {
//    val input = "fn f: Int = 12345 >> 0"
//    val model = new Flix().addStr(input).solve().get
//    val result = model.constants(Name.Resolved.mk("f"))
//    assertResult(Value.mkInt32(12345 >> 0))(result)
//  }
//
//  test("Interpreter - BinaryOperator.Less01") {
//    val input = Expression.Binary(
//      BinaryOperator.Less,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Less02") {
//    val input = Expression.Binary(
//      BinaryOperator.Less,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Less03") {
//    val input = Expression.Binary(
//      BinaryOperator.Less,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Less04") {
//    val input = Expression.Binary(
//      BinaryOperator.Less,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Less05") {
//    val input = Expression.Binary(
//      BinaryOperator.Less,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Less06") {
//    val input = Expression.Binary(
//      BinaryOperator.Less,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.LessEqual01") {
//    val input = Expression.Binary(
//      BinaryOperator.LessEqual,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.LessEqual02") {
//    val input = Expression.Binary(
//      BinaryOperator.LessEqual,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.LessEqual03") {
//    val input = Expression.Binary(
//      BinaryOperator.LessEqual,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.LessEqual04") {
//    val input = Expression.Binary(
//      BinaryOperator.LessEqual,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.LessEqual05") {
//    val input = Expression.Binary(
//      BinaryOperator.LessEqual,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.LessEqual06") {
//    val input = Expression.Binary(
//      BinaryOperator.LessEqual,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Greater01") {
//    val input = Expression.Binary(
//      BinaryOperator.Greater,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Greater02") {
//    val input = Expression.Binary(
//      BinaryOperator.Greater,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Greater03") {
//    val input = Expression.Binary(
//      BinaryOperator.Greater,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Greater04") {
//    val input = Expression.Binary(
//      BinaryOperator.Greater,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Greater05") {
//    val input = Expression.Binary(
//      BinaryOperator.Greater,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Greater06") {
//    val input = Expression.Binary(
//      BinaryOperator.Greater,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.GreaterEqual01") {
//    val input = Expression.Binary(
//      BinaryOperator.GreaterEqual,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.GreaterEqual02") {
//    val input = Expression.Binary(
//      BinaryOperator.GreaterEqual,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.GreaterEqual03") {
//    val input = Expression.Binary(
//      BinaryOperator.GreaterEqual,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.GreaterEqual04") {
//    val input = Expression.Binary(
//      BinaryOperator.GreaterEqual,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.GreaterEqual05") {
//    val input = Expression.Binary(
//      BinaryOperator.GreaterEqual,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.GreaterEqual06") {
//    val input = Expression.Binary(
//      BinaryOperator.GreaterEqual,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Equal01") {
//    val input = Expression.Binary(
//      BinaryOperator.Equal,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Equal02") {
//    val input = Expression.Binary(
//      BinaryOperator.Equal,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Equal03") {
//    val input = Expression.Binary(
//      BinaryOperator.Equal,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Equal04") {
//    val input = Expression.Binary(
//      BinaryOperator.Equal,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Equal05") {
//    val input = Expression.Binary(
//      BinaryOperator.Equal,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Equal06") {
//    val input = Expression.Binary(
//      BinaryOperator.Equal,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.NotEqual01") {
//    val input = Expression.Binary(
//      BinaryOperator.NotEqual,
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.NotEqual02") {
//    val input = Expression.Binary(
//      BinaryOperator.NotEqual,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.NotEqual03") {
//    val input = Expression.Binary(
//      BinaryOperator.NotEqual,
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.NotEqual04") {
//    val input = Expression.Binary(
//      BinaryOperator.NotEqual,
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.NotEqual05") {
//    val input = Expression.Binary(
//      BinaryOperator.NotEqual,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-12, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.NotEqual06") {
//    val input = Expression.Binary(
//      BinaryOperator.NotEqual,
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Expression.Lit(Literal.Int(-3, loc), Type.Int32, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.And01") {
//    val input = Expression.Binary(
//      BinaryOperator.LogicalAnd,
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.And02") {
//    val input = Expression.Binary(
//      BinaryOperator.LogicalAnd,
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.And03") {
//    val input = Expression.Binary(
//      BinaryOperator.LogicalAnd,
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Or01") {
//    val input = Expression.Binary(
//      BinaryOperator.LogicalOr,
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Or02") {
//    val input = Expression.Binary(
//      BinaryOperator.LogicalOr,
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.True)(result)
//  }
//
//  test("Interpreter - BinaryOperator.Or03") {
//    val input = Expression.Binary(
//      BinaryOperator.LogicalOr,
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Type.Bool, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.False)(result)
//  }
//
//  /////////////////////////////////////////////////////////////////////////////
//  // Expressions - If Then Else                                              //
//  /////////////////////////////////////////////////////////////////////////////
//
//  test("Interpreter - Expression.IfThenElse01") {
//    val input = Expression.IfThenElse(
//      Expression.Lit(Literal.Bool(true, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Str("foo", loc), Type.Str, loc),
//      Expression.Lit(Literal.Str("bar", loc), Type.Str, loc),
//      Type.Str, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("foo"))(result)
//  }
//
//  test("Interpreter - Expression.IfThenElse02") {
//    val input = Expression.IfThenElse(
//      Expression.Lit(Literal.Bool(false, loc), Type.Bool, loc),
//      Expression.Lit(Literal.Str("foo", loc), Type.Str, loc),
//      Expression.Lit(Literal.Str("bar", loc), Type.Str, loc),
//      Type.Str, loc
//    )
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("bar"))(result)
//  }
//
//  test("Interpreter - Expression.IfThenElse03") {
//    // if (20 % 7 >= 3 || 25 - 5 == 4) "foo" else "bar"
//    val input = Expression.IfThenElse(
//      Expression.Binary(
//        BinaryOperator.LogicalOr,
//        Expression.Binary(
//          BinaryOperator.GreaterEqual,
//          Expression.Binary(
//            BinaryOperator.Modulo,
//            Expression.Lit(Literal.Int(20, loc), Type.Int32, loc),
//            Expression.Lit(Literal.Int(7, loc), Type.Int32, loc),
//            Type.Int32, loc
//          ),
//          Expression.Lit(Literal.Int(3, loc), Type.Int32, loc),
//          Type.Bool
//          , loc),
//        Expression.Binary(
//          BinaryOperator.Equal,
//          Expression.Binary(
//            BinaryOperator.Minus,
//            Expression.Lit(Literal.Int(25, loc), Type.Int32, loc),
//            Expression.Lit(Literal.Int(5, loc), Type.Int32, loc),
//            Type.Int32, loc
//          ),
//          Expression.Lit(Literal.Int(4, loc), Type.Int32, loc),
//          Type.Bool
//          , loc),
//        Type.Bool, loc
//      ),
//      Expression.Lit(Literal.Str("foo", loc), Type.Str, loc),
//      Expression.Lit(Literal.Str("bar", loc), Type.Str, loc),
//      Type.Str
//      , loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkStr("foo"))(result)
//  }
//
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
//  test("Interpreter - Expression.Tag01") {
//    val name = Name.Resolved.mk(List("foo", "bar"))
//    val ident = toIdent("baz")
//    val tagTpe = Type.Tag(name, ident, Type.Str)
//    val enumTpe = Type.Enum(name, Map("foo.bar.baz" -> tagTpe))
//    val input = Expression.Tag(name, ident,
//      // if (!(4 != 4)) "hello world" else "asdfasdf"
//      Expression.IfThenElse(
//        Expression.Unary(
//          UnaryOperator.LogicalNot,
//          Expression.Binary(
//            BinaryOperator.NotEqual,
//            Expression.Lit(Literal.Int(4, loc), Type.Int32, loc),
//            Expression.Lit(Literal.Int(4, loc), Type.Int32, loc),
//            Type.Bool, loc),
//          Type.Bool, loc),
//        Expression.Lit(Literal.Str("hello world", loc), Type.Str, loc),
//        Expression.Lit(Literal.Str("asdfasdf", loc), Type.Str, loc),
//        Type.Str, loc),
//      enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "baz", Value.mkStr("hello world")))(result)
//  }
//
//  test("Interpreter - Expression.Tag02") {
//    val name = Name.Resolved.mk(List("Family"))
//    val ident = toIdent("NameAndAge")
//    val tagTpe = Type.Tag(name, ident, Type.Tuple(List(Type.Str, Type.Int32)))
//    val enumTpe = Type.Enum(name, Map("Family.NameAndAge" -> tagTpe))
//    val input = Expression.Tag(name, ident, Expression.Tuple(List(
//      Expression.Lit(Literal.Str("James", loc), Type.Str, loc),
//      // 20 + 22
//      Expression.Binary(
//        BinaryOperator.Plus,
//        Expression.Lit(Literal.Int(20, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(22, loc), Type.Int32, loc),
//        Type.Int32, loc)),
//      Type.Tuple(List(Type.Str, Type.Int32)), loc), enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "NameAndAge", Value.Tuple(Array(Value.mkStr("James"), Value.mkInt32(42)))))(result)
//  }
//
//  test("Interpreter - Expression.Tag03") {
//    import ConstantPropTagDefs._
//    val input = Expression.Tag(name, identB, Expression.Lit(Literal.Unit(loc), Type.Unit, loc), enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Bot", Value.Unit))(result)
//  }
//
//  test("Interpreter - Expression.Tag04") {
//    import ConstantPropTagDefs._
//    val input = Expression.Tag(name, identT, Expression.Lit(Literal.Unit(loc), Type.Unit, loc), enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Top", Value.Unit))(result)
//  }
//
//  test("Interpreter - Expression.Tag05") {
//    import ConstantPropTagDefs._
//    val input = Expression.Tag(name, identV,
//      // 123 - 123
//      Expression.Binary(
//        BinaryOperator.Minus,
//        Expression.Lit(Literal.Int(123, loc), Type.Int32, loc),
//        Expression.Lit(Literal.Int(123, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(0)))(result)
//  }
//
//  test("Interpreter - Expression.Tag06") {
//    import ConstantPropTagDefs._
//    val input = Expression.Tag(name, identV,
//      // -240
//      Expression.Unary(
//        UnaryOperator.Minus,
//        Expression.Lit(Literal.Int(240, loc), Type.Int32, loc),
//        Type.Int32, loc),
//      enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(-240)))(result)
//  }
//
//  test("Interpreter - Expression.Tag07") {
//    import ConstantPropTagDefs._
//    val input = Expression.Tag(name, identV, Expression.Lit(Literal.Int(1241, loc), Type.Int32, loc), enumTpe, loc)
//    val result = Interpreter.eval(input, root)
//    assertResult(Value.mkTag(name, "Val", Value.mkInt32(1241)))(result)
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
