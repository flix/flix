package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source, SymbolSet}
import ca.uwaterloo.flix.language.ast.Symbol
import org.scalatest.funsuite.AnyFunSuite

class SymbolSetSuite extends AnyFunSuite {
  def sameSymbolSet(s1: SymbolSet, s2: SymbolSet): Boolean= {
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

}
