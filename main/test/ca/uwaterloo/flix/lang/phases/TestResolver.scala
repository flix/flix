package ca.uwaterloo.flix.lang.phases

import ca.uwaterloo.flix.lang.ast.{ResolvedAst, ParsedAst, SourceLocation, WeededAst}
import ca.uwaterloo.flix.lang.phase.{Resolver, Parser, Weeder}
import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class TestResolver extends FunSuite {

  val Ident = ParsedAst.Ident("x", SourceLocation.Unknown)

  test("Literal.Tag01") {
    val wast = WeededAst.Literal.Tag(
      name = ParsedAst.QName(Seq("SomeName", "OtherName"), SourceLocation.Unknown),
      ident = ParsedAst.Ident("TagName", SourceLocation.Unknown),
      literal = WeededAst.Literal.Unit)

    val namespace = List("A", "B", "C")
    val globals = Map(
      ResolvedAst.RName(List("SomeName", "OtherName")) -> WeededAst.Definition.Enum(Ident, Map.empty)
    )

    val result = Resolver.Literal.link(wast, namespace, globals)
    assert(result.get.isInstanceOf[ResolvedAst.Literal.Tag])
  }

  test("Literal.Tag02") {
    val wast = WeededAst.Literal.Tag(
      name = ParsedAst.QName(Seq("D"), SourceLocation.Unknown),
      ident = ParsedAst.Ident("TagName", SourceLocation.Unknown),
      literal = WeededAst.Literal.Unit)

    val namespace = List("A", "B", "C")
    val globals = Map(
      ResolvedAst.RName(List("A", "B", "C", "D")) -> WeededAst.Definition.Enum(Ident, Map.empty)
    )

    val result = Resolver.Literal.link(wast, namespace, globals)
    assert(result.get.isInstanceOf[ResolvedAst.Literal.Tag])
  }
}
