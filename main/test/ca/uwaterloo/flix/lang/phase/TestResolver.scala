package ca.uwaterloo.flix.lang.phase

import ca.uwaterloo.flix.lang.ast._
import ca.uwaterloo.flix.lang.phase.{Resolver, Parser, Weeder}
import org.scalatest.FunSuite

import scala.collection.immutable.Seq

class TestResolver extends FunSuite {

  val Ident = ParsedAst.Ident("x", SourceLocation.Unknown)

  test("Literal.Tag01") {
//    val wast = WeededAst.Literal.Tag(
//      name = ParsedAst.QName(Seq("SomeName", "OtherName"), SourceLocation.Unknown),
//      ident = ParsedAst.Ident("TagName", SourceLocation.Unknown),
//      literal = WeededAst.Literal.Unit)
//
//    val namespace = List("A", "B", "C")
//    val globals = Map(
//      Name.Resolved(List("SomeName", "OtherName"), SourceLocation.Unknown) -> WeededAst.Definition.Enum(Ident, Map.empty)
//    )
//
//    val result = Resolver.Literal.resolve(wast, namespace, globals)
//    assert(result.get.isInstanceOf[ResolvedAst.Literal.Tag])
  }

  test("Literal.Tag02") {
//    val wast = WeededAst.Literal.Tag(
//      name = ParsedAst.QName(Seq("D"), SourceLocation.Unknown),
//      ident = ParsedAst.Ident("TagName", SourceLocation.Unknown),
//      literal = WeededAst.Literal.Unit)
//
//    val namespace = List("A", "B", "C")
//    val globals = Map(
//      Name.Resolved(List("A", "B", "C", "D"), SourceLocation.Unknown) -> WeededAst.Definition.Enum(Ident, Map.empty)
//    )
//
//    val result = Resolver.Literal.resolve(wast, namespace, globals)
//    assert(result.get.isInstanceOf[ResolvedAst.Literal.Tag])
  }
}
