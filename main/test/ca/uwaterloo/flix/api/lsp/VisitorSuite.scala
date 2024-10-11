/*
 * Copyright 2024 Alexander Dybdahl Troelsen
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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.api.lsp.provider.Visitor
import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition}
import org.scalatest.funsuite.AnyFunSuite


class VisitorSuite extends AnyFunSuite {
  val source = Source(Input.Text("test", "test", true, SecurityContext.AllPermissions), Array.emptyCharArray)
  val uri = "test"

  test("Inside.01") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 3, 10),
      SourcePosition(source, 6, 2))
    val pos = Position(5, 5)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.02") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 5, 2),
      SourcePosition(source, 5, 5),
    )
    val pos = Position(5, 4)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.03") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 5, 4),
      SourcePosition(source, 5, 5),
    )
    val pos = Position(5, 4)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.04") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 5),
    )
    val pos = Position(6, 4)

    assert(Visitor.inside(uri, pos)(loc) === true)
  }

  test("Inside.05") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 4, 4),
      SourcePosition(source, 6, 10),
    )
    val pos = Position(3, 7)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.06") {

    val loc = SourceLocation(
      false,
      SourcePosition(source, 3, 2),
      SourcePosition(source, 6, 5),
    )
    val pos = Position(7, 4)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.07") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 2, 7),
      SourcePosition(source, 6, 4),
    )
    val pos = Position(6, 11)

    assert(Visitor.inside(uri, pos)(loc) === false)

  }

  test("Inside.08") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 2, 7),
      SourcePosition(source, 6, 4),
    )
    val pos = Position(2, 3)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.09") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 6),
    )
    val pos = Position(6, 2)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }

  test("Inside.10") {
    val loc = SourceLocation(
      false,
      SourcePosition(source, 3, 5),
      SourcePosition(source, 5, 3),
    )
    val pos = Position(5, 3)

    assert(Visitor.inside(uri, pos)(loc) === false)
  }
}
