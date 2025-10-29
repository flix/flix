/*
 * Copyright 2025 Asher Frost
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.language.ast

import ca.uwaterloo.flix.TestUtils
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source, SymbolSet}
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
}
