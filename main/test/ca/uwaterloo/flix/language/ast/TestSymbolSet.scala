/*
 * Copyright 2025 Asher Frost
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.{SecurityContext, Source, SymbolSet}
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.funsuite.AnyFunSuite

class TestSymbolSet extends AnyFunSuite with TestUtils {
  def sameSymbolSet(s1: SymbolSet, s2: SymbolSet): Boolean = {
    s1.enums == s2.enums &&
      s1.structs == s2.structs &&
      s1.traits == s2.traits &&
      s1.effects == s2.effects
  }

  test("`ambiguous` unit test a") {
    val testSetA = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.c")), Set.empty, Set.empty, Set.empty
    )
    val testSetB = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("A.b"), Symbol.mkEnumSym("C.b")), Set.empty, Set.empty, Set.empty
    )
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set(
      Symbol.mkEnumSym("A.b"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.b")), Set.empty, Set.empty, Set.empty
    )
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` unit test b") {
    val testSetA = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.c")), Set.empty, Set.empty, Set.empty
    )
    val testSetB = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.c")), Set.empty, Set.empty, Set.empty
    )
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set.empty, Set.empty, Set.empty, Set.empty)
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` unit test c") {
    val testSetA = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.c")), Set.empty, Set.empty, Set.empty
    )
    val testSetB = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.d")), Set.empty, Set.empty, Set.empty
    )
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set.empty, Set.empty, Set.empty, Set.empty)
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` unit test d") {
    val testSetA = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.c")), Set.empty, Set.empty, Set.empty
    )
    val testSetB = SymbolSet(Set(
      Symbol.mkEnumSym("A.a"), Symbol.mkEnumSym("B.b"), Symbol.mkEnumSym("C.c")), Set.empty, Set.empty, Set.empty
    )
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set.empty, Set.empty, Set.empty, Set.empty)
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` two enums with the same name") {
    val testSetA = SymbolSet(Set(Symbol.mkEnumSym("A.Color")), Set.empty, Set.empty, Set.empty)
    val testSetB = SymbolSet(Set(Symbol.mkEnumSym("B.Color")), Set.empty, Set.empty, Set.empty)
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set(Symbol.mkEnumSym("A.Color"), Symbol.mkEnumSym("B.Color")), Set.empty, Set.empty, Set.empty)
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` two structs with the same name") {
    val testSetA = SymbolSet(Set.empty, Set(mkStructSym("A", "Point")), Set.empty, Set.empty)
    val testSetB = SymbolSet(Set.empty, Set(mkStructSym("B", "Point")), Set.empty, Set.empty)
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set.empty, Set(mkStructSym("A", "Point"), mkStructSym("B", "Point")), Set.empty, Set.empty)
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` two traits with the same name") {
    val testSetA = SymbolSet(Set.empty, Set.empty, Set(Symbol.mkTraitSym("A.Show")), Set.empty)
    val testSetB = SymbolSet(Set.empty, Set.empty, Set(Symbol.mkTraitSym("B.Show")), Set.empty)
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set.empty, Set.empty, Set(Symbol.mkTraitSym("A.Show"), Symbol.mkTraitSym("B.Show")), Set.empty)
    assert(sameSymbolSet(result, expected))
  }

  test("`ambiguous` two effects with the same name") {
    val testSetA = SymbolSet(Set.empty, Set.empty, Set.empty, Set(Symbol.mkEffSym("A.Log")))
    val testSetB = SymbolSet(Set.empty, Set.empty, Set.empty, Set(Symbol.mkEffSym("B.Log")))
    val result = SymbolSet.ambiguous(testSetA, testSetB)
    val expected = SymbolSet(Set.empty, Set.empty, Set.empty, Set(Symbol.mkEffSym("A.Log"), Symbol.mkEffSym("B.Log")))
    assert(sameSymbolSet(result, expected))
  }

  /**
    * Returns the struct symbol with the given `name` in the namespace `ns`.
    */
  private def mkStructSym(ns: String, name: String): Symbol.StructSym =
    new Symbol.StructSym(None, List(ns), name, SourceLocation.Unknown)
}
