package ca.uwaterloo.flix.lang.phases

import ca.uwaterloo.flix.lang.ast._
import ca.uwaterloo.flix.lang.phase.Typer
import org.scalatest.FunSuite

class TestTyper extends FunSuite {

  val Root = ResolvedAst.Root(Map.empty, List.empty, List.empty)
  val Ident = ParsedAst.Ident("x", SourceLocation.Unknown)
  val RName = Name.Resolved(List("foo", "bar"))

  // TODO: At some point it might be worth just compiling program fragments?

  /////////////////////////////////////////////////////////////////////////////
  // Literals                                                                //
  /////////////////////////////////////////////////////////////////////////////
  test("Literal.Unit") {
    val rast = ResolvedAst.Literal.Unit
    val result = Typer.Literal.typer(rast, Root)
    assertResult(TypedAst.Type.Unit)(result.tpe)
  }

  test("Literal.Bool.True") {
    val rast = ResolvedAst.Literal.Bool(true)
    val result = Typer.Literal.typer(rast, Root)
    assertResult(TypedAst.Type.Bool)(result.tpe)
  }

  test("Literal.Bool.False") {
    val rast = ResolvedAst.Literal.Bool(false)
    val result = Typer.Literal.typer(rast, Root)
    assertResult(TypedAst.Type.Bool)(result.tpe)
  }

  test("Literal.Int") {
    val rast = ResolvedAst.Literal.Int(42)
    val result = Typer.Literal.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.tpe)
  }

  test("Literal.Str") {
    val rast = ResolvedAst.Literal.Str("foo")
    val result = Typer.Literal.typer(rast, Root)
    assertResult(TypedAst.Type.Str)(result.tpe)
  }

  test("Literal.Tag.Unit") {
    val enumName = Name.Resolved(List("foo", "bar", "baz"))
    val tagName = ParsedAst.Ident("Qux", SourceLocation.Unknown)
    val literal = ResolvedAst.Literal.Unit
    val rast = ResolvedAst.Literal.Tag(enumName, tagName, literal)
    val enums = Map(enumName -> ResolvedAst.Definition.Enum(enumName, Map("Qux" -> ResolvedAst.Type.Tag(enumName, tagName, ResolvedAst.Type.Unit))))
    val root = Root.copy(enums = enums)
    val result = Typer.Literal.typer(rast, root)
    assertResult(TypedAst.Type.Enum(Map("Qux" -> TypedAst.Type.Tag(enumName, tagName, TypedAst.Type.Unit))))(result.tpe)
  }

  test("Literal.Tuple") {
    val rast = ResolvedAst.Literal.Tuple(List(
      ResolvedAst.Literal.Bool(true),
      ResolvedAst.Literal.Int(42),
      ResolvedAst.Literal.Str("foo")))
    val result = Typer.Literal.typer(rast, Root)
    assertResult(TypedAst.Type.Tuple(List(TypedAst.Type.Bool, TypedAst.Type.Int, TypedAst.Type.Str)))(result.tpe)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Expressions                                                             //
  /////////////////////////////////////////////////////////////////////////////
  test("Expression.Unary01") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.Not, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)))
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Bool)(result.get.tpe)
  }

  test("Expression.Unary02") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.UnaryPlus, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42)))
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.get.tpe)
  }

  test("Expression.Unary03") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.UnaryMinus, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42)))
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.get.tpe)
  }

  test("Expression.Unary.NonBooleanValue") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.Not, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42)))
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Unary.NonIntegerValue01") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.UnaryPlus, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)))
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Unary.NonIntegerValue02") {
    val rast = ResolvedAst.Expression.Unary(UnaryOperator.UnaryMinus, ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)))
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Binary01") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(false))
    val rast = ResolvedAst.Expression.Binary(BinaryOperator.And, e1, e2)
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Bool)(result.get.tpe)
  }

  test("Expression.Binary02") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(21))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    val rast = ResolvedAst.Expression.Binary(BinaryOperator.Minus, e1, e2)
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.get.tpe)
  }

  test("Expression.Binary03") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(21))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    val rast = ResolvedAst.Expression.Binary(BinaryOperator.LessEqual, e1, e2)
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Bool)(result.get.tpe)
  }

  test("Expression.Binary.MismatchedValues") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    val rast = ResolvedAst.Expression.Binary(BinaryOperator.Plus, e1, e2)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.Binary.MismatchedOperator") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(false))
    val rast = ResolvedAst.Expression.Binary(BinaryOperator.Plus, e1, e2)
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.IfThenElse01") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(21)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    )
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.get.tpe)
  }

  test("Expression.IfThenElse02") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(false)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("a")),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("b"))
    )
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Str)(result.get.tpe)
  }

  test("Expression.IfThenElse.NonBooleanCondition") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(1)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(2)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(3))
    )
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  test("Expression.IfThenElse.ThenElseMismatch") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(2)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo"))
    )
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }

  // TODO
  test("Expression.Let01") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(2)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo"))
    )
    val result = Typer.Expression.typer(rast, Root)
    assert(result.isFailure)
  }


  test("Expression.Tuple01") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    val rast = ResolvedAst.Expression.Tuple(List(e1, e2))
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Tuple(List(TypedAst.Type.Bool, TypedAst.Type.Int)))(result.get.tpe)
  }

  test("Expression.Tuple02") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true))
    val e2 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    val e3 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Str("foo"))
    val rast = ResolvedAst.Expression.Tuple(List(e1, e2, e3))
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Tuple(List(TypedAst.Type.Bool, TypedAst.Type.Int, TypedAst.Type.Str)))(result.get.tpe)
  }

  test("Expression.Tuple03") {
    val e1 = ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true))
    val e2 = ResolvedAst.Expression.Tuple(List(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Unit)
    ))
    val rast = ResolvedAst.Expression.Tuple(List(e1, e2))
    val result = Typer.Expression.typer(rast, Root)
    val tpe1 = TypedAst.Type.Bool
    val tpe2 = TypedAst.Type.Tuple(List(TypedAst.Type.Unit, TypedAst.Type.Unit))
    assertResult(TypedAst.Type.Tuple(List(tpe1, tpe2)))(result.get.tpe)
  }

  test("Expression.Error01") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Error(SourceLocation.Unknown),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(21)),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    )
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.get.tpe)
  }

  test("Expression.Error02") {
    val rast = ResolvedAst.Expression.IfThenElse(
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Bool(true)),
      ResolvedAst.Expression.Error(SourceLocation.Unknown),
      ResolvedAst.Expression.Lit(ResolvedAst.Literal.Int(42))
    )
    val result = Typer.Expression.typer(rast, Root)
    assertResult(TypedAst.Type.Int)(result.get.tpe)
  }


  /////////////////////////////////////////////////////////////////////////////
  // Types                                                                   //
  /////////////////////////////////////////////////////////////////////////////
  test("Type.Unit") {
    val rast = ResolvedAst.Type.Unit
    val result = Typer.Type.typer(rast)
    assertResult(TypedAst.Type.Unit)(result)
  }

  test("Type.Bool") {
    val rast = ResolvedAst.Type.Bool
    val result = Typer.Type.typer(rast)
    assertResult(TypedAst.Type.Bool)(result)
  }

  test("Type.Int") {
    val rast = ResolvedAst.Type.Int
    val result = Typer.Type.typer(rast)
    assertResult(TypedAst.Type.Int)(result)
  }

  test("Type.Str") {
    val rast = ResolvedAst.Type.Str
    val result = Typer.Type.typer(rast)
    assertResult(TypedAst.Type.Str)(result)
  }

  test("Type.Tag") {
    val rast = ResolvedAst.Type.Tag(RName, Ident, ResolvedAst.Type.Unit)
    val result = Typer.Type.typer(rast)
    assertResult(TypedAst.Type.Tag(RName, Ident, TypedAst.Type.Unit))(result)
  }

  test("Type.Enum") {
    ??? // TODO
  }

  test("Type.Tuple") {
    val rtype1 = ResolvedAst.Type.Bool
    val rtype2 = ResolvedAst.Type.Int
    val rast = ResolvedAst.Type.Tuple(List(rtype1, rtype2))
    val tpe1 = TypedAst.Type.Bool
    val tpe2 = TypedAst.Type.Int
    val result = Typer.Type.typer(rast)
    assertResult(TypedAst.Type.Tuple(List(tpe1, tpe2)))(result)
  }

  test("Type.Function") {
    ??? // TODO
  }

}
