package ca.uwaterloo.flix.lang

import ca.uwaterloo.flix.lang.ast.{Ast, Compiler}
import impl.logic.BinaryOperator
import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class TestParser extends FunSuite {

  test("Parser.LiteralExp01") {
    val s = """val e: Bool = true;"""
    val a = Ast.Expression.Lit(Ast.Literal.Bool(true))

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LiteralExp02") {
    val s = """val e: Bool = false;"""
    val a = Ast.Expression.Lit(Ast.Literal.Bool(false))

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LiteralExp03") {
    val s = """val e: Int = 42;"""
    val a = Ast.Expression.Lit(Ast.Literal.Int(42))

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LiteralExp04") {
    val s = """val e: Str = "abc";"""
    val a = Ast.Expression.Lit(Ast.Literal.Str("abc"))

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LogicalExp01") {
    val s = """val e: Bool = a && b;"""
    val a = Ast.Expression.Binary("a", BinaryOperator.And, "b")

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LogicalExp02") {
    val s = """val e: Bool = a && b && c;"""
    val a = Ast.Expression.Binary(
      Ast.Expression.Binary("a", BinaryOperator.And, "b"),
      BinaryOperator.And,
      "c")

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.ComparisonExp01") {
    val s = """val e: Bool = a == b;"""
    val a = Ast.Expression.Binary("a", BinaryOperator.Equal, "b")

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.BinaryExp01") {
    val s = """val e: Bool = a == b || c == d;"""
    val a = Ast.Expression.Binary(
      Ast.Expression.Binary("a", BinaryOperator.Equal, "b"),
      BinaryOperator.Or,
      Ast.Expression.Binary("c", BinaryOperator.Equal, "d")
    )

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LetExp01") {
    val s = """val e: Int = let x = 1 in 2;"""
    val a = Ast.Expression.Let("x", Ast.Expression.Lit(Ast.Literal.Int(1)), Ast.Expression.Lit(Ast.Literal.Int(2)))

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.IfThenElseExp01") {
    val s = """val e: Int = if (a) b else c;"""
    val a = Ast.Expression.IfThenElse("a", "b", "c")

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.LetExp02") {
    val s =
      """val e: Int =
        |  let x = 1 in
        |  let y = 2 in
        |    3;""".stripMargin
    val a = Ast.Expression.Let("x", Ast.Expression.Lit(
      Ast.Literal.Int(1)),
      Ast.Expression.Let("y",
        Ast.Expression.Lit(Ast.Literal.Int(2)),
        Ast.Expression.Lit(Ast.Literal.Int(3))))

    assertResult(a)(getExp(Parser.parse(s)))
  }

  test("Parser.Type.Unit") {
    val s = """type t = Unit;"""
    val a = Ast.Type.Unit

    assertResult(a)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Bool") {
    val s = """type t = Bool;"""
    val t = Ast.Type.Bool

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Int") {
    val s = """type t = Int;"""
    val t = Ast.Type.Int

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Str") {
    val s = """type t = Str;"""
    val t = Ast.Type.Str

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple01") {
    val s = """type t = (Bool, Bool);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Bool, Ast.Type.Bool))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple02") {
    val s = """type t = (Bool, Bool, Bool);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Bool, Ast.Type.Bool, Ast.Type.Bool))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple03") {
    val s = """type t = (Bool, Int, Str);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple04") {
    val s = """type t = (Unit, Bool, Int, Str, Unit, Bool, Int, Str);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Unit, Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str, Ast.Type.Unit, Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Tuple05") {
    val s = """type t = (Unit);"""
    val t = Ast.Type.Tuple(Seq(Ast.Type.Unit))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set01") {
    val s = """type t = Set[Unit];"""
    val t = Ast.Type.Set(Ast.Type.Unit)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set02") {
    val s = """type t = Set[Bool];"""
    val t = Ast.Type.Set(Ast.Type.Bool)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set03") {
    val s = """type t = Set[Int];"""
    val t = Ast.Type.Set(Ast.Type.Int)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Set04") {
    val s = """type t = Set[Set[Int]];"""
    val t = Ast.Type.Set(Ast.Type.Set(Ast.Type.Int))

    assertResult(t)(getType(Parser.parse(s)))
  }

//  test("Parser.Type.Map01") {
//    val s = """type t = Map[Bool, Bool];"""
//    val t = Ast.Type.Map(Seq(Ast.Type.Bool, Ast.Type.Bool))
//
//    assertResult(t)(getType(Parser.parse(s)))
//  }
//
//  test("Parser.Type.Map02") {
//    val s = """type t = Map[Int, Int];"""
//    val t = Ast.Type.Map(Seq(Ast.Type.Int, Ast.Type.Int))
//
//    assertResult(t)(getType(Parser.parse(s)))
//  }
//
//  test("Parser.Type.Map03") {
//    val s = """type t = Map[Bool, Int, Str];"""
//    val t = Ast.Type.Map(Seq(Ast.Type.Bool, Ast.Type.Int, Ast.Type.Str))
//
//    assertResult(t)(getType(Parser.parse(s)))
//  }

  test("Parser.Type.Enum01") {
    val s =
      """type t = enum {
        |case A
        |};""".stripMargin
    val t = Ast.Type.Enum(Seq(Ast.Type.Tag("A")))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Enum02") {
    val s =
      """type t = enum {
        |case A,
        |case B
        |};""".stripMargin
    val t = Ast.Type.Enum(Seq(Ast.Type.Tag("A"), Ast.Type.Tag("B")))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Function01") {
    val s = """type t = Unit -> Unit;"""
    val t = Ast.Type.Function(Ast.Type.Unit, Ast.Type.Unit)

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Function02") {
    val s = """type t = Unit -> Unit -> Unit;"""
    val t = Ast.Type.Function(Ast.Type.Unit,
      Ast.Type.Function(Ast.Type.Unit, Ast.Type.Unit))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.Type.Function03") {
    val s = """type t = Unit -> Bool -> Int -> Str;"""
    val t = Ast.Type.Function(Ast.Type.Unit,
      Ast.Type.Function(Ast.Type.Bool, Ast.
        Type.Function(Ast.Type.Int, Ast.Type.Str)))

    assertResult(t)(getType(Parser.parse(s)))
  }

  test("Parser.NameSpace01") {
    val s = """namespace a {};"""
    val a = Ast.Root(Seq(Ast.Declaration.NameSpace(Seq("a"), Seq.empty)))

    assertResult(a)(Parser.parse(s))
  }

  test("Parser.NameSpace02") {
    val s = """namespace a.b {};"""
    val a = Ast.Root(Seq(Ast.Declaration.NameSpace(Seq("a", "b"), Seq.empty)))

    assertResult(a)(Parser.parse(s))
  }

  test("Parser.NameSpace03") {
    val s = """namespace a.b.b.a {};"""
    val a = Ast.Root(Seq(Ast.Declaration.NameSpace(Seq("a", "b", "b", "a"), Seq.empty)))

    assertResult(a)(Parser.parse(s))
  }

  test("Parser.NameSpace04") {
    val s =
      """namespace a {
        |  namespace b {};
        |  namespace c {};
        |};""".stripMargin
    val a = Ast.Root(Seq(Ast.Declaration.NameSpace(Seq("a"), Seq(
      Ast.Declaration.NameSpace(Seq("b"), Seq.empty),
      Ast.Declaration.NameSpace(Seq("c"), Seq.empty)
    ))))

    assertResult(a)(Parser.parse(s))
  }

  test("Parser.Error01") {
    val s = """val e: Bool = ???;"""
    val a = Ast.Root(Seq(Ast.Declaration.Val("e", Ast.Type.Bool, Ast.Expression.Error)))

    assertResult(a)(Parser.parse(s))
  }
/*

  ignore("Parser.Call01") {
    val s = """val e: Bool = f(1);"""
    val a = Ast.Root(Seq(Ast.Declaration.Val("e", Ast.Type.Bool,
      Ast.Expression.Call(Ast.Expression.AmbiguousName(Seq("f")), Seq(Ast.Expression.Lit(Ast.Literal.Int(1)))))))

    assertResult(a)(Parser.parse(s))
  }

  ignore("Parser.Call02") {
    val s = """val e: Bool = f(1, 2, 3);"""
    val a = Ast.Root(Seq(Ast.Declaration.Val("e", Ast.Type.Bool,
      Ast.Expression.Call(Ast.Expression.AmbiguousName(Seq("f")), Seq(Ast.Expression.Lit(Ast.Literal.Int(1)))))))

    assertResult(a)(Parser.parse(s))
  }

  ignore("Parser.Call03") {
    val s = """val e: Bool = f(g(h(x, y, z);"""
    val a = Ast.Root(Seq(Ast.Declaration.Val("e", Ast.Type.Bool,
      Ast.Expression.Call(Ast.Expression.AmbiguousName(Seq("f")), Seq(Ast.Expression.Lit(Ast.Literal.Int(1)))))))

    assertResult(a)(Parser.parse(s))
  }
*/


  test("Parser.Lattice01") {
    val s = """var e: {Int};"""
    //val a = Ast.Root(Seq(Ast.Declaration.Var("e", ???);

    //assertResult(a)(Parser.parse(s))
  }


  private def getExp(root: Ast.Root): Ast.Expression = root match {
    case Ast.Root(Seq(Ast.Declaration.Val(_, _, e))) => e
  }

  private def getType(root: Ast.Root): Ast.Type = Compiler.compile(root) match {
    case Ast.Root(Seq(Ast.Declaration.Tpe(_, typ))) => typ
    case _ => throw new RuntimeException()
  }

  private implicit def string2nameexp(s: String): Ast.Expression.AmbiguousName =
    Ast.Expression.AmbiguousName(Seq(s))

}
