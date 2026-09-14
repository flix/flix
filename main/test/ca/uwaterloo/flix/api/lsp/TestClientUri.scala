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
package ca.uwaterloo.flix.api.lsp

import ca.uwaterloo.flix.language.ast.shared.SourceName
import org.scalatest.funsuite.AnyFunSuite

import java.net.URI
import java.nio.file.Path

/**
  * Documents which client URIs survive the round trip through [[ClientUri]] on the platform the
  * tests run on. A shape that does not survive here is one the language servers will report in
  * their shadow, and one the switch to the conversion has to normalize first.
  */
class TestClientUri extends AnyFunSuite {

  private def roundTrip(uri: String): Option[String] = ClientUri.toSourceName(uri).map(ClientUri.fromSourceName)

  private val absolute = Path.of("").toAbsolutePath

  test("ClientUri.RoundTrip.PlainFile") {
    val uri = absolute.resolve("proj/Main.flix").toUri.toString
    assert(uri.startsWith("file:///"))
    assert(roundTrip(uri).contains(uri))
  }

  test("ClientUri.RoundTrip.FileWithSpaces") {
    val uri = absolute.resolve("my proj/Main.flix").toUri.toString
    assert(uri.contains("%20"))
    assert(roundTrip(uri).contains(uri))
  }

  test("ClientUri.RoundTrip.FileWithNonAscii") {
    val uri = absolute.resolve("prøj/Main.flix").toUri.toString
    assert(uri.contains("%C3%B8"))
    assert(roundTrip(uri).contains(uri))
  }

  test("ClientUri.RoundTrip.UntitledBuffer") {
    val uri = "untitled:Untitled-1"
    assert(ClientUri.toSourceName(uri).contains(SourceName.UriName(new URI(uri))))
    assert(roundTrip(uri).contains(uri))
  }

  test("ClientUri.RoundTrip.RelativePath") {
    // The compiler's own names, e.g. the test file and the library, have no scheme and are sent as is.
    val uri = "__TEST__.flix"
    assert(ClientUri.toSourceName(uri).contains(SourceName.PathName(Path.of(uri))))
    assert(roundTrip(uri).contains(uri))
  }

  test("ClientUri.RoundTrip.WindowsDriveLetterAsVSCodeSendsIt") {
    // VS Code on Windows sends a lowercase drive letter with the colon percent-encoded. Java does not
    // reproduce that string, on any platform. This is the shape the switch has to normalize.
    val uri = "file:///c%3A/Users/me/Main.flix"
    val back = roundTrip(uri)
    assert(back.isDefined)
    assert(!back.contains(uri), s"unexpectedly round-tripped: $back")
  }

  test("ClientUri.Malformed") {
    assert(ClientUri.toSourceName("file:///a b/Main.flix").isEmpty)
  }

  test("ClientUri.Shadow.RecordsEachMismatchOnce") {
    val uri = "file:///c%3A/Users/me/Shadow.flix"
    val ok = absolute.resolve("Shadow.flix").toUri.toString
    ClientUri.shadow(ok)
    ClientUri.shadow(uri)
    ClientUri.shadow(uri)
    ClientUri.shadow("file:///a b/Shadow.flix")
    val recorded = ClientUri.shadowMismatches
    assert(!recorded.exists(_._1 == ok))
    assert(recorded.count(_._1 == uri) == 1)
    assert(recorded.exists { case (u, back) => u == "file:///a b/Shadow.flix" && back.isEmpty })
  }

}
