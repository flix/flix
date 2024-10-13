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

import ca.uwaterloo.flix.language.ast.shared.{Input, SecurityContext, Source}
import ca.uwaterloo.flix.language.ast.{SourceLocation, SourcePosition}
import org.scalatest.funsuite.AnyFunSuite

class VisitorSuite extends AnyFunSuite {
  val source: Source = Source(Input.Text("test", "test", stable = true, SecurityContext.AllPermissions), Array.emptyCharArray)
  val uri = "test"

  test("inside when strictly within lines") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 10),
      SourcePosition(source, 6, 2)
    )
    val pos = Position(5, 5)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on start line and after start character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 4),
      SourcePosition(source, 5, 2)
    )
    val  pos = Position(3, 5)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on start line and at start character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 4),
      SourcePosition(source, 5, 2)
    )
    val  pos = Position(3, 4)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on end line and before end character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 4),
      SourcePosition(source, 5, 3)
    )
    val  pos = Position(5, 1)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on end line and right before end character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 4),
      SourcePosition(source, 5, 3)
    )
    val  pos = Position(5, 2)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on start and end line and within characters") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 5, 2),
      SourcePosition(source, 5, 10)
    )
    val  pos = Position(5, 4)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on start and end line and at start character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 5, 2),
      SourcePosition(source, 5, 10)
    )
    val  pos = Position(5, 2)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("inside when on start and end line and right before end character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 5, 2),
      SourcePosition(source, 5, 10)
    )
    val  pos = Position(5, 9)

    assert(Visitor.inside(uri, pos)(loc))
  }

  test("not inside when before start line") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 4, 4),
      SourcePosition(source, 6, 10),
    )
    val pos = Position(3, 7)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside when after end line") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 2),
      SourcePosition(source, 6, 5),
    )
    val pos = Position(7, 4)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside when on start line but before start character (column)") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 7),
      SourcePosition(source, 6, 4),
    )
    val pos = Position(2, 6)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside when on end line but after end character (column)") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 2, 7),
      SourcePosition(source, 6, 4),
    )
    val pos = Position(6, 11)

    assert(!Visitor.inside(uri, pos)(loc))

  }

  test("not inside when on end line and on end character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 5),
      SourcePosition(source, 5, 3),
    )
    val pos = Position(5, 3)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside when on start and end line but before start character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 8)
    )
    val pos = Position(6, 3)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside when on start and end line but after end character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 8)
    )
    val pos = Position(6, 9)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside when on start and end line but on end character") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 6, 4),
      SourcePosition(source, 6, 8)
    )
    val pos = Position(6, 8)

    assert(!Visitor.inside(uri, pos)(loc))
  }

  test("not inside if uri doesn't match source") {
    val loc = SourceLocation(
      isReal = true,
      SourcePosition(source, 3, 6),
      SourcePosition(source, 6, 10)
    )
    val pos = Position(4, 4)

    assert(!Visitor.inside("wrong!", pos)(loc))
  }

  test("not inside if SourceLocation isn't real") {
    val loc = SourceLocation(
      isReal = false,
      SourcePosition(source, 3, 6),
      SourcePosition(source, 6, 10)
    )
    val pos = Position(4, 4)

    assert(!Visitor.inside(uri, pos)(loc))
  }
}
