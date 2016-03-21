package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.{Flix, IValue, Invokable}
import org.scalatest.FunSuite

class TestResolver extends FunSuite {

  test("DuplicateDefinition01") {
    val input =
      s"""
         |fn f: Int = 42
         |fn f: Int = 21
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.DuplicateDefinition])
  }

  test("DuplicateDefinition02") {
    val input =
      s"""
         |fn f: Int = 42
         |fn f: Int = 21
         |fn f: Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.DuplicateDefinition])
  }

  test("DuplicateDefinition03") {
    val input =
      s"""
         |fn f(x: Int): Int = 42
         |fn f(x: Int): Int = 21
         |fn f(x: Int): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.DuplicateDefinition])
  }

  test("DuplicateDefinition04") {
    val input =
      s"""
         |fn f: Int = 42
         |fn f(x: Int): Int = 21
         |fn f(x: Bool, y: Int, z: String): Int = 11
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.DuplicateDefinition])
  }

  test("DuplicateDefinition05") {
    val input =
      s"""
         |namespace A {
         |  fn f: Int = 42
         |}
         |
         |namespace A {
         |  fn f: Int = 21
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.DuplicateDefinition])
  }

  test("DuplicateDefinition06") {
    val input =
      s"""
         |namespace A.B.C {
         |  fn f: Int = 42
         |}
         |
         |namespace A {
         |  namespace B {
         |    namespace C {
         |      fn f: Int = 21
         |    }
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.DuplicateDefinition])
  }

  test("IllegalConstantName01") {
    val input = "fn F: Int = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalConstantName])
  }

  test("IllegalConstantName02") {
    val input =
      s"""
         |namespace A {
         |  fn Foo: Int = 42
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalConstantName])
  }

  test("IllegalConstantName03") {
    val input =
      s"""
         |namespace A {
         |  fn FOO: Int = 42
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalConstantName])
  }

  test("IllegalConstantName04") {
    val input =
      s"""
         |namespace A {
         |  fn F(x: Int): Int = 42
         |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalConstantName])
  }

  test("IllegalRelationName01") {
    val input =
      s"""
         |namespace A {
         |  rel f(x: Int)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalRelationName])
  }

  test("IllegalRelationName02") {
    val input =
      s"""
         |namespace A {
         |  rel foo(x: Int)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalRelationName])
  }

  test("IllegalRelationName03") {
    val input =
      s"""
         |namespace A {
         |  rel fOO(x: Int)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.IllegalRelationName])
  }

  test("UnresolvedConstantReference01") {
    val input =
      s"""
         |namespace A {
         |  fn f: Int = x;
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedConstantReference])
  }

  test("UnresolvedConstantReference02") {
    val input =
      s"""
         |namespace A {
         |  fn f(x: Int, y: Int): Int = x + y + z;
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedConstantReference])
  }

  test("UnresolvedEnumReference01") {
    val input =
      s"""
         |namespace A {
         |  fn f: Int = Foo.Bar
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedEnumReference])
  }

  test("UnresolvedEnumReference02") {
    val input =
      s"""
         |namespace A {
         |  fn f: Int = Foo/Bar.Qux(true)
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedEnumReference])
  }

  test("UnresolvedTagReference01") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  fn f: B = B.Qux;
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTagReference])
  }

  test("UnresolvedTagReference02") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  fn f: B = B.Qux(1 + 2);
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTagReference])
  }

  test("UnresolvedTagReference03") {
    val input =
      s"""
         |namespace A {
         |  enum B {
         |    case Foo,
         |    case Bar
         |  }
         |
         |  fn f(b: B): Int = match b with {
         |    case B.Qux => 42;
         |  }
         |}
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTagReference])
  }

  test("UnresolvedRelationReference01") {
    val input =
      s"""namespace A {
          |  VarPointsTo(1, 2).
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedRelationReference])
  }

  test("UnresolvedRelationReference02") {
    val input =
      s"""namespace A {
          |  VarPointsTo(1, 2).
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedRelationReference])
  }

  test("UnresolvedTypeReference01") {
    val input = "fn x: Foo = 42"
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTypeReference])
  }

  test("UnresolvedTypeReference02") {
    val input =
      s"""namespace A {
          |  def foo(bar: Baz, baz: Baz): Qux = bar;
          |};
       """.stripMargin
    val result = new Flix().addStr(input).compile()
    assert(result.errors.head.isInstanceOf[Resolver.ResolverError.UnresolvedTypeReference])
  }

  test("Expression.Hook01") {
    val input =
      s"""namespace A {
          |  fn f(x: Int): Bool = g(x)
          |};
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type), flix.mkBoolType)
    flix
      .addStr(input)
      .addHook("A/g", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    flix.compile().get
  }

  test("Expression.Hook02") {
    val input =
      s"""namespace A {
          |  fn f(x: Bool, y: Int, z: Str): Bool = g(x, y, z)
          |};
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkInt32Type, flix.mkStrType), flix.mkBoolType)
    flix
      .addStr(input)
      .addHook("A/g", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    flix.compile().get
  }

  test("Expression.HookFilter01") {
    val input =
      s"""namespace A {
          |  rel R(a: Bool, b: Int, c: Str);
          |
          |  R(x, y, z) :- f(x, y, z), R(x, y, z).
          |};
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkBoolType, flix.mkInt32Type, flix.mkStrType), flix.mkBoolType)
    flix
      .addStr(input)
      .addHook("A/f", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkTrue
      })
    val result = flix.compile()
    assert(result.isSuccess)
  }

  test("Expression.HookApply01") {
    val input =
      s"""namespace A {
          |  rel R(a: Bool, b: Int, c: Str);
          |
          |  R(x, y, f(x, y, z)) :- R(x, y, z).
          |};
       """.stripMargin
    val flix = new Flix()
    val tpe = flix.mkFunctionType(Array(flix.mkInt32Type, flix.mkStrType), flix.mkStrType)
    flix
      .addStr(input)
      .addHook("A/f", tpe, new Invokable {
        def apply(args: Array[IValue]) = flix.mkStr("foo")
      })
    val result = flix.compile()
    assert(result.isSuccess)
  }

}
