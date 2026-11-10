/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.lsp.SemanticTokenType
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.funsuite.AnyFunSuite

class TestFmt extends AnyFunSuite {

  private val optionSym: Symbol.EnumSym = Symbol.mkEnumSym("Option")

  private val orderSym: Symbol.TraitSym = Symbol.mkTraitSym("Order")

  /**
    * The tree of `Option[Int32] -> Bool`.
    */
  private val arrow: Fmt =
    Fmt.Ref(optionSym, SemanticTokenType.Enum, "Option") ++ Fmt.text("[") ++ Fmt.ty("Int32") ++ Fmt.text("]") ++
      Fmt.text(" ") ++ Fmt.op("->") ++ Fmt.text(" ") ++ Fmt.ty("Bool")

  /**
    * An emitter that collects the symbols of the references, in order.
    */
  private object SymbolEmitter extends Emitter[List[Symbol]] {

    def text(s: String): List[Symbol] = Nil

    def token(kind: SemanticTokenType, s: String): List[Symbol] = Nil

    def ref(sym: Symbol, kind: SemanticTokenType, s: String): List[Symbol] = List(sym)

    def concat(parts: List[List[Symbol]]): List[Symbol] = parts.flatten

  }

  test("Fmt.Plain.Text.01") {
    val expected = ", "
    val actual = Fmt.text(", ").plain

    assert(actual == expected)
  }

  test("Fmt.Plain.Token.01") {
    val expected = "def"
    val actual = Fmt.kw("def").plain

    assert(actual == expected)
  }

  test("Fmt.Plain.Ref.01") {
    val expected = "Option"
    val actual = Fmt.Ref(optionSym, SemanticTokenType.Enum, "Option").plain

    assert(actual == expected)
  }

  test("Fmt.Plain.Ref.02") {
    // A reference is written with the text it is given, not with the name of its symbol.
    val expected = "A.B.Option"
    val actual = Fmt.Ref(optionSym, SemanticTokenType.Enum, "A.B.Option").plain

    assert(actual == expected)
  }

  test("Fmt.Plain.Concat.01") {
    val expected = "Option[Int32] -> Bool"
    val actual = arrow.plain

    assert(actual == expected)
  }

  test("Fmt.Plain.Empty.01") {
    val expected = ""
    val actual = Fmt.Empty.plain

    assert(actual == expected)
  }

  test("Fmt.Plain.Empty.02") {
    val expected = "def"
    val actual = (Fmt.Empty ++ Fmt.kw("def") ++ Fmt.Empty).plain

    assert(actual == expected)
  }

  test("Fmt.Tokens.Text.01") {
    val expected = Nil
    val actual = Fmt.text(", ").tokens

    assert(actual == expected)
  }

  test("Fmt.Tokens.Token.01") {
    val expected = List(
      (SemanticTokenType.Keyword, "def"),
      (SemanticTokenType.Operator, "->"),
      (SemanticTokenType.Type, "Int32"),
      (SemanticTokenType.Effect, "Pure"),
      (SemanticTokenType.TypeParameter, "a"),
      (SemanticTokenType.Property, "x")
    )
    val actual = (Fmt.kw("def") ++ Fmt.op("->") ++ Fmt.ty("Int32") ++ Fmt.ef("Pure") ++ Fmt.tparam("a") ++ Fmt.prop("x")).tokens

    assert(actual == expected)
  }

  test("Fmt.Tokens.Ref.01") {
    val expected = List((SemanticTokenType.Interface, "Order"))
    val actual = Fmt.Ref(orderSym, SemanticTokenType.Interface, "Order").tokens

    assert(actual == expected)
  }

  test("Fmt.Tokens.Concat.01") {
    // The punctuation and the spacing are left out.
    val expected = List(
      (SemanticTokenType.Enum, "Option"),
      (SemanticTokenType.Type, "Int32"),
      (SemanticTokenType.Operator, "->"),
      (SemanticTokenType.Type, "Bool")
    )
    val actual = arrow.tokens

    assert(actual == expected)
  }

  test("Fmt.Tokens.Empty.01") {
    val expected = Nil
    val actual = Fmt.Empty.tokens

    assert(actual == expected)
  }

  test("Fmt.Sep.01") {
    val expected = ""
    val actual = Fmt.sep(", ", Nil).plain

    assert(actual == expected)
  }

  test("Fmt.Sep.02") {
    val expected = "Int32"
    val actual = Fmt.sep(", ", List(Fmt.ty("Int32"))).plain

    assert(actual == expected)
  }

  test("Fmt.Sep.03") {
    val expected = "Int32, String, Bool"
    val actual = Fmt.sep(", ", List(Fmt.ty("Int32"), Fmt.ty("String"), Fmt.ty("Bool"))).plain

    assert(actual == expected)
  }

  test("Fmt.Sep.04") {
    val and = Fmt.text(" ") ++ Fmt.kw("and") ++ Fmt.text(" ")
    val tree = Fmt.sep(and, List(Fmt.tparam("a"), Fmt.tparam("b"), Fmt.tparam("c")))

    assert(tree.plain == "a and b and c")
  }

  test("Fmt.Sep.05") {
    // A separator that is a tree keeps its tokens, unlike punctuation.
    val and = Fmt.text(" ") ++ Fmt.kw("and") ++ Fmt.text(" ")
    val tree = Fmt.sep(and, List(Fmt.tparam("a"), Fmt.tparam("b")))

    val expected = List(
      (SemanticTokenType.TypeParameter, "a"),
      (SemanticTokenType.Keyword, "and"),
      (SemanticTokenType.TypeParameter, "b")
    )
    val actual = tree.tokens

    assert(actual == expected)
  }

  test("Fmt.Parens.01") {
    val expected = "(Option[Int32] -> Bool)"
    val actual = Fmt.parens(arrow).plain

    assert(actual == expected)
  }

  test("Fmt.Parens.02") {
    // The parentheses are punctuation.
    val expected = arrow.tokens
    val actual = Fmt.parens(arrow).tokens

    assert(actual == expected)
  }

  test("Fmt.Render.01") {
    // An emitter is given the symbol of every reference, in order.
    val tree = Fmt.Ref(orderSym, SemanticTokenType.Interface, "Order") ++ Fmt.text("[") ++ arrow ++ Fmt.text("]")

    val expected = List(orderSym, optionSym)
    val actual = Fmt.render(tree, SymbolEmitter)

    assert(actual == expected)
  }

  test("Fmt.Render.02") {
    val expected = Nil
    val actual = Fmt.render(Fmt.kw("def") ++ Fmt.text(" ") ++ Fmt.tparam("a"), SymbolEmitter)

    assert(actual == expected)
  }

}
