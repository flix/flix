/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.shared.{RegionScope, SymbolSet, VarText}
import ca.uwaterloo.flix.language.ast.{Kind, Name, SourceLocation, Symbol, Type, TypeConstructor}
import org.scalatest.funsuite.AnyFunSuite

class TestPretty extends AnyFunSuite {

  private val scope = RegionScope.Top
  private val loc = SourceLocation.Unknown
  private implicit val fmt: FormatOptions = FormatOptions(FormatOptions.VarName.NameBased)

  private val a: Type = mkVar(1, "a", Kind.Star)
  private val b: Type = mkVar(2, "b", Kind.Star)
  private val ef: Type = mkVar(3, "ef", Kind.Eff)
  private val listSym: Symbol.EnumSym = Symbol.mkEnumSym("List")
  private val listOfA: Type = Type.mkEnum(listSym, List(a), loc)

  /** No link, no class: the HTML of a type is its text, escaped. */
  private val noLink: Symbol => Option[String] = _ => None
  private val noClass: SemanticTokenType => Option[String] = _ => None

  private def mkVar(id: Int, text: String, kind: Kind): Type =
    Type.Var(new Symbol.KindedTypeVarSym(id, VarText.SourceText(text), kind, scope, loc), loc)

  test("Pretty.Tokens.Enum.01") {
    val f = Pretty.tpe(listOfA)
    assert(f.plain == "List[a]")
    assert(f.tokens == List((SemanticTokenType.Enum, "List"), (SemanticTokenType.TypeParameter, "a")))
  }

  test("Pretty.Tokens.Arrow.01") {
    val f = Pretty.tpe(Type.mkArrowWithEffect(a, ef, b, loc))
    assert(f.plain == raw"a -> b \ ef")
    assert(f.tokens == List(
      (SemanticTokenType.TypeParameter, "a"),
      (SemanticTokenType.Operator, "->"),
      (SemanticTokenType.TypeParameter, "b"),
      (SemanticTokenType.Operator, "\\"),
      (SemanticTokenType.TypeParameter, "ef")))
  }

  test("Pretty.Tokens.Record.01") {
    val f = Pretty.tpe(Type.mkRecord(Type.mkRecordRowExtend(Name.Label("x", loc), Type.Int32, Type.RecordRowEmpty, loc), loc))
    assert(f.plain == "{ x = Int32 }")
    assert(f.tokens == List((SemanticTokenType.Property, "x"), (SemanticTokenType.Type, "Int32")))
  }

  test("Pretty.Tokens.Not.01") {
    val f = Pretty.tpe(Type.mkNot(ef, loc))
    assert(f.plain == "not ef")
    assert(f.tokens == List((SemanticTokenType.Keyword, "not"), (SemanticTokenType.TypeParameter, "ef")))
  }

  test("Pretty.Tokens.Effect.01") {
    val f = Pretty.tpe(Type.IO)
    assert(f.plain == "IO")
    assert(f.tokens == List((SemanticTokenType.Effect, "IO")))
  }

  test("Pretty.Delimit.01") {
    // An operator is parenthesized in argument position; an atom is not.
    val union = Type.mkUnion(ef, mkVar(4, "ef2", Kind.Eff), loc)
    assert(Pretty.tpe(Type.mkArrowWithEffect(union, Type.Pure, b, loc)).plain == "(ef + ef2) -> b")
    assert(Pretty.tpe(Type.mkArrowWithEffect(listOfA, Type.Pure, b, loc)).plain == "List[a] -> b")
  }

  test("Pretty.Eff.Pure.01") {
    assert(Pretty.tpe(Type.Pure).plain == "Pure")
    assert(Pretty.eff(Type.Pure).plain == "{}")
    assert(Pretty.eff(Type.IO).plain == "IO")
  }

  test("Pretty.EffSuffix.01") {
    assert(Pretty.effSuffix(Type.Pure).plain == "")
    assert(Pretty.effSuffix(Type.IO).plain == raw" \ IO")
    assert(Pretty.effSuffix(Type.mkUnion(Type.IO, ef, loc)).plain == raw" \ IO + ef")
  }

  test("Pretty.TypeAndEff.01") {
    assert(Pretty.typeAndEff(Type.Int32, Type.Pure).plain == "Int32")
    assert(Pretty.typeAndEff(Type.Int32, Type.IO).plain == raw"Int32 \ IO")
  }

  test("Pretty.TypeAndEff.02") {
    // With `simplify`, an effect formula that is pure leaves no suffix.
    val e = Type.mkUnion(ef, ef, loc)
    assert(Pretty.typeAndEff(Type.Int32, e, simplify = true).plain == raw"Int32 \ ef")
    assert(Pretty.typeAndEff(Type.Int32, Type.mkIntersection(ef, Type.mkComplement(ef, loc), loc), simplify = true).plain == "Int32")
  }

  test("Pretty.Qualify.01") {
    val sym = Symbol.mkEnumSym("A.B.Color")
    val tpe = Type.mkEnum(sym, Nil, loc)
    assert(Pretty.tpe(tpe).plain == "Color")
    val qualified = fmt.copy(qualify = SymbolSet.empty.copy(enums = Set(sym)))
    assert(Pretty.tpe(tpe)(qualified).plain == "A.B.Color")
    assert(Pretty.tpe(tpe)(qualified).tokens == List((SemanticTokenType.Enum, "A.B.Color")))
  }

  test("Pretty.VarNames.01") {
    val t = Type.Var(new Symbol.KindedTypeVarSym(7, VarText.SourceText("x"), Kind.Star, scope, loc), loc)
    assert(Pretty.tpe(t).plain == "x")
    assert(Pretty.tpe(t)(FormatOptions.Internal).plain == "t7")
    assert(Pretty.tpe(Type.Var(new Symbol.KindedTypeVarSym(7, VarText.Absent, Kind.Eff, scope, loc), loc)).plain == "e7")
  }

  test("Pretty.Html.01") {
    // A resolvable reference is a link; a token is a span.
    val link: Symbol => Option[String] = { case `listSym` => Some("List.html"); case _ => None }
    val cssClass: SemanticTokenType => Option[String] = {
      case SemanticTokenType.Enum => Some("ty")
      case SemanticTokenType.TypeParameter => Some("ty")
      case _ => None
    }
    assert(Pretty.tpe(listOfA).html(link, cssClass) == "<a class='ty' href='List.html'>List</a>[<span class='ty'>a</span>]")
  }

  test("Pretty.Html.02") {
    // An unresolvable reference degrades to a token, not a dead link.
    val cssClass: SemanticTokenType => Option[String] = { case SemanticTokenType.Enum => Some("ty"); case _ => None }
    assert(Pretty.tpe(listOfA).html(noLink, cssClass) == "<span class='ty'>List</span>[a]")
  }

  test("Pretty.Html.03") {
    // Text and tokens are escaped.
    assert(Pretty.tpe(Type.mkArrowWithEffect(a, Type.Pure, b, loc)).html(noLink, noClass) == "a -&gt; b")
  }

  test("Pretty.Html.04") {
    // A URL is escaped as an attribute value.
    val link: Symbol => Option[String] = _ => Some("A.html?x=1&y='2'")
    assert(Pretty.tpe(listOfA).html(link, noClass) == "<a href='A.html?x=1&amp;y=&#39;2&#39;'>List</a>[a]")
  }

  test("Pretty.Tycon.01") {
    assert(Pretty.tycon(TypeConstructor.Arrow(2)).plain == "Arrow2")
    assert(Pretty.tycon(TypeConstructor.Enum(listSym, Kind.Star)).tokens == List((SemanticTokenType.Enum, "List")))
    assert(Pretty.tycon(TypeConstructor.RecordRowExtend(Name.Label("x", loc))).plain == "RecordRowExtend(x)")
  }

}
