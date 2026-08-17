/*
 * Copyright 2026 Werner Stein
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.util.{CollisionRegistry, InternalCompilerException, Options, StableName}
import org.scalatest.funsuite.AnyFunSuite

class TestSymbol extends AnyFunSuite {

  private def flixWithWidth(width: Int): Flix = new Flix().setOptions(Options.Default.copy(xstableNameLength = width))

  private def hashOf(id: Option[SymId]): String = id match {
    case Some(SymId.Hash(value)) => value
    case other => fail(s"expected a content-addressed id, got $other")
  }

  test("specializedDefnSym.RespectsWidth.01") {
    val enclosing = Symbol.mkDefnSym("List.map")
    for (width <- List(1, 4, 8, 12, StableName.MaxWidth)) {
      implicit val flix: Flix = flixWithWidth(width)
      val sym = Symbol.specializedDefnSym(enclosing, "List.map|Int32")
      assert(hashOf(sym.id) == StableName.suffix("List.map|Int32", width))
    }
  }

  test("specializedDefnSym.WidensByNesting.01") {
    // Mirrors the nesting property StableName itself guarantees: the narrower
    // suffix is always the trailing digits of the wider one, for the same key.
    val enclosing = Symbol.mkDefnSym("List.map")
    val narrowSym = Symbol.specializedDefnSym(enclosing, "key")(flixWithWidth(4))
    val wideSym = Symbol.specializedDefnSym(enclosing, "key")(flixWithWidth(12))
    assert(hashOf(wideSym.id).endsWith(hashOf(narrowSym.id)))
  }

  test("specializedDefnSym.OptOut.01") {
    // 0 opts out of content-addressing entirely: the resulting id is a Counter,
    // not a Hash, exactly as if this were minted before the scheme existed.
    implicit val flix: Flix = flixWithWidth(0)
    val enclosing = Symbol.mkDefnSym("f")
    val sym = Symbol.specializedDefnSym(enclosing, "key")
    sym.id match {
      case Some(SymId.Counter(_)) => // expected
      case other => fail(s"expected a Counter id, got $other")
    }
  }

  test("specializedDefnSym.OptOut.02") {
    // Each opted-out call still consumes a fresh GenSym id, so two specializations
    // of the same key do not collapse onto the same symbol.
    implicit val flix: Flix = flixWithWidth(0)
    val enclosing = Symbol.mkDefnSym("f")
    val first = Symbol.specializedDefnSym(enclosing, "key")
    val second = Symbol.specializedDefnSym(enclosing, "key")
    assert(first.id != second.id)
  }

  test("liftedDefnSym.RespectsWidth.01") {
    val enclosing = Symbol.mkDefnSym("f")
    implicit val flix: Flix = flixWithWidth(6)
    val sym = Symbol.liftedDefnSym(enclosing, 2)
    assert(hashOf(sym.id) == StableName.suffix(s"$enclosing#lift2", 6))
  }

  test("specializedEnumSym.RespectsWidth.01") {
    val enclosing = Symbol.mkEnumSym("List")
    implicit val flix: Flix = flixWithWidth(6)
    val sym = Symbol.specializedEnumSym(enclosing, "List[Int32]")
    assert(hashOf(sym.id) == StableName.suffix("List[Int32]", 6))
  }

  test("specializedStructSym.RespectsWidth.01") {
    val enclosing = new Symbol.StructSym(None, Nil, "S", SourceLocation.Unknown)
    implicit val flix: Flix = flixWithWidth(6)
    val sym = Symbol.specializedStructSym(enclosing, "S[Int32]")
    assert(hashOf(sym.id) == StableName.suffix("S[Int32]", 6))
  }

  test("specializedAnonClassSym.RespectsWidth.01") {
    val enclosing = Symbol.mkDefnSym("f")
    implicit val flix: Flix = flixWithWidth(6)
    val sym = Symbol.specializedAnonClassSym(enclosing, 0, SourceLocation.Unknown)
    assert(hashOf(sym.id) == StableName.suffix(s"$enclosing#anon0", 6))
  }

  // The opt-out below is exercised only through specializedDefnSym above; these confirm
  // the other four callers of the same private stableOrCounterId helper opt out too, so a
  // future refactor that duplicates rather than shares the helper can't silently drop it
  // for one of them.

  test("liftedDefnSym.OptOut.01") {
    implicit val flix: Flix = flixWithWidth(0)
    val enclosing = Symbol.mkDefnSym("f")
    val sym = Symbol.liftedDefnSym(enclosing, 2)
    sym.id match {
      case Some(SymId.Counter(_)) => // expected
      case other => fail(s"expected a Counter id, got $other")
    }
  }

  test("specializedEnumSym.OptOut.01") {
    implicit val flix: Flix = flixWithWidth(0)
    val enclosing = Symbol.mkEnumSym("List")
    val sym = Symbol.specializedEnumSym(enclosing, "List[Int32]")
    sym.id match {
      case Some(SymId.Counter(_)) => // expected
      case other => fail(s"expected a Counter id, got $other")
    }
  }

  test("specializedStructSym.OptOut.01") {
    implicit val flix: Flix = flixWithWidth(0)
    val enclosing = new Symbol.StructSym(None, Nil, "S", SourceLocation.Unknown)
    val sym = Symbol.specializedStructSym(enclosing, "S[Int32]")
    sym.id match {
      case Some(SymId.Counter(_)) => // expected
      case other => fail(s"expected a Counter id, got $other")
    }
  }

  test("specializedAnonClassSym.OptOut.01") {
    implicit val flix: Flix = flixWithWidth(0)
    val enclosing = Symbol.mkDefnSym("f")
    val sym = Symbol.specializedAnonClassSym(enclosing, 0, SourceLocation.Unknown)
    sym.id match {
      case Some(SymId.Counter(_)) => // expected
      case other => fail(s"expected a Counter id, got $other")
    }
  }

  test("specializedDefnSym.PigeonholeCollisionAtWidth1.01") {
    // Width 1 has exactly 36 possible base-36 values. Feeding more than 36 distinct keys
    // for the *same* enclosing (namespace, text) -- exactly what Namer/Deriver/Eraser/
    // Specialization do for 37+ specializations of one generic def -- guarantees two of
    // them share an id by pigeonhole, not by chance: at least one collision is certain,
    // not merely likely. This exercises the real SHA-256 hash and the real
    // CollisionRegistry Namer/Deriver/Eraser/Specialization all claim through, rather
    // than a synthetic (key, value) pair engineered to already agree.
    implicit val flix: Flix = flixWithWidth(1)
    val enclosing = Symbol.mkDefnSym("f")
    val registry = new CollisionRegistry[Symbol.DefnSym, String]()

    val thrown = intercept[InternalCompilerException] {
      for (i <- 0 until 37) {
        val key = s"specialization-key-$i"
        val sym = Symbol.specializedDefnSym(enclosing, key)
        registry.claim(sym, key, SourceLocation.Unknown)((existing, incoming) =>
          s"id collision on '$sym': '$existing' and '$incoming'."
        )
      }
    }
    assert(thrown.getMessage.contains("collision"))
  }

  test("specializedDefnSym.NoCollisionAtWidth1WithFewKeys.01") {
    // The flip side, so the guarantee above isn't taken on faith: claiming distinct keys
    // must not throw merely because the registry is exercised at all. "specialization-key-0"
    // through "-9" are hardcoded, not arbitrary or random: verified by direct computation
    // to render to 10 pairwise-distinct base-36 digits at width 1 (x, v, e, h, s, u, f, r,
    // 5, c), so this is a deterministic non-collision, on the same footing as a golden
    // vector -- not a probabilistic claim that happens to pass today.
    implicit val flix: Flix = flixWithWidth(1)
    val enclosing = Symbol.mkDefnSym("f")
    val registry = new CollisionRegistry[Symbol.DefnSym, String]()

    for (i <- 0 until 10) {
      val key = s"specialization-key-$i"
      val sym = Symbol.specializedDefnSym(enclosing, key)
      registry.claim(sym, key, SourceLocation.Unknown)((existing, incoming) =>
        fail(s"unexpected collision for a 10-key sample: '$existing' vs '$incoming'")
      )
    }
  }

  test("specializedDefnSym.RejectsOutOfRangeWidth.01") {
    // Options is directly constructible outside the CLI (tests, LSP, library embedding),
    // so a caller can reach here with a width the CLI's own validate() would have rejected.
    // Pinned here since it currently surfaces as StableName.of's require, an
    // IllegalArgumentException rather than the compiler's usual InternalCompilerException.
    implicit val flix: Flix = flixWithWidth(-1)
    val enclosing = Symbol.mkDefnSym("f")
    assertThrows[IllegalArgumentException] {
      Symbol.specializedDefnSym(enclosing, "key")
    }
  }

}
