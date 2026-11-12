/*
 * Copyright 2025 Asher Frost
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.SymUse.{AssocTypeSymUse, TraitSymUse}
import ca.uwaterloo.flix.language.ast.shared.{EqualityConstraint, SecurityContext, Source, SymbolSet, TraitConstraint}
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

  test("`symbolsOf` an associated type") {
    val result = SymbolSet.symbolsOf(Type.AssocType(elm, color, Kind.Star, loc))
    val expected = SymbolSet(Set(Symbol.mkEnumSym("B.Color")), Set.empty, Set(Symbol.mkTraitSym("A.Coll")), Set.empty)
    assert(result == expected)
  }

  test("`symbolsOf` an associated type inside a type") {
    val result = SymbolSet.symbolsOf(Type.mkArrowWithEffect(Type.Int32, Type.Pure, Type.AssocType(elm, Type.Int32, Kind.Star, loc), loc))
    val expected = SymbolSet(Set.empty, Set.empty, Set(Symbol.mkTraitSym("A.Coll")), Set.empty)
    assert(result == expected)
  }

  test("`symbolsOf` a trait constraint") {
    val result = SymbolSet.symbolsOf(show)
    val expected = SymbolSet(Set(Symbol.mkEnumSym("B.Color")), Set.empty, Set(Symbol.mkTraitSym("A.Show")), Set.empty)
    assert(result == expected)
  }

  test("`symbolsOf` an equality constraint") {
    val result = SymbolSet.symbolsOf(elmIsColor)
    val expected = SymbolSet(Set(Symbol.mkEnumSym("B.Color")), Set.empty, Set(Symbol.mkTraitSym("A.Coll")), Set.empty)
    assert(result == expected)
  }

  test("`symbolsOf` a scheme") {
    val point = Type.Cst(TypeConstructor.Struct(mkStructSym("C", "Point"), Kind.Star), loc)
    val result = SymbolSet.symbolsOf(Scheme(Nil, List(show), List(elmIsColor), point))
    val expected = SymbolSet(
      Set(Symbol.mkEnumSym("B.Color")),
      Set(mkStructSym("C", "Point")),
      Set(Symbol.mkTraitSym("A.Show"), Symbol.mkTraitSym("A.Coll")),
      Set.empty
    )
    assert(result == expected)
  }

  test("`ambiguities` two associated types of two traits with the same name") {
    val elmOfB = AssocTypeSymUse(Symbol.mkAssocTypeSym(Symbol.mkTraitSym("B.Coll"), Name.Ident("Elm", loc)), loc)
    val result = SymbolSet.ambiguities(Type.AssocType(elm, Type.Int32, Kind.Star, loc), Type.AssocType(elmOfB, Type.Int32, Kind.Star, loc))
    val expected = SymbolSet(Set.empty, Set.empty, Set(Symbol.mkTraitSym("A.Coll"), Symbol.mkTraitSym("B.Coll")), Set.empty)
    assert(result == expected)
  }

  private val loc: SourceLocation = SourceLocation.Unknown

  /**
    * The enum type `B.Color`.
    */
  private val color: Type = Type.Cst(TypeConstructor.Enum(Symbol.mkEnumSym("B.Color"), Kind.Star), loc)

  /**
    * The associated type `A.Coll.Elm`.
    */
  private val elm: AssocTypeSymUse = AssocTypeSymUse(Symbol.mkAssocTypeSym(Symbol.mkTraitSym("A.Coll"), Name.Ident("Elm", loc)), loc)

  /**
    * The trait constraint `A.Show[B.Color]`.
    */
  private val show: TraitConstraint = TraitConstraint(TraitSymUse(Symbol.mkTraitSym("A.Show"), loc), color, loc)

  /**
    * The equality constraint `A.Coll.Elm[Int32] ~ B.Color`.
    */
  private val elmIsColor: EqualityConstraint = EqualityConstraint(elm, Type.Int32, color, loc)

  /**
    * Returns the struct symbol with the given `name` in the namespace `ns`.
    */
  private def mkStructSym(ns: String, name: String): Symbol.StructSym =
    new Symbol.StructSym(None, List(ns), name, SourceLocation.Unknown)
}
