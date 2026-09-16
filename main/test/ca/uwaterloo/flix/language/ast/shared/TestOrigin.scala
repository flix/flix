/*
 * Copyright 2026 Magnus Madsen
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
package ca.uwaterloo.flix.language.ast.shared

import org.scalatest.funsuite.AnyFunSuite

import java.lang.constant.ClassDesc

class TestOrigin extends AnyFunSuite {

  /** Identifiers that differ only in the characters the root passes through unchanged. */
  private val Ids: List[String] = List(
    "github:flix/museum-clerk",
    "github:jls/tic-tac-toe",
    "github:ababup1192/flix_game_engine",
    "github:a/b-c",
    "github:a/b_c",
    "github:a-b/c",
  )

  test("Origin.canonicalRoot.01") {
    assertResult(expected = "$pkg$github$jls$tic-tac-toe")(actual = Origin.canonicalRoot("github:jls/tic-tac-toe"))
  }

  test("Origin.canonicalRoot.02") {
    // Distinct identifiers have distinct roots, every root starts with `$` so that it cannot be
    // written in source, and every root is a legal JVM package path.
    val roots = Ids.map(Origin.canonicalRoot)
    assertResult(expected = Ids.length)(actual = roots.distinct.length)
    assert(roots.forall(_.startsWith("$")), roots)
    roots.foreach(root => ClassDesc.ofInternalName(root + "/Main"))
  }

}
