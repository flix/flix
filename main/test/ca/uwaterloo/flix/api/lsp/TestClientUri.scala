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

class TestClientUri extends AnyFunSuite {

  private def roundTrip(uri: String): Option[String] = ClientUri.toSourceName(uri).map(ClientUri.fromSourceName)

  private val absolute = Path.of("").toAbsolutePath

  test("ClientUri.FileUriNamesPath") {
    val path = absolute.resolve("proj/Main.flix")
    assert(ClientUri.toSourceName(path.toUri.toString).contains(SourceName.PathName(path)))
  }

  test("ClientUri.FileUriWithSpacesNamesPath") {
    val path = absolute.resolve("my proj/Main.flix")
    val uri = path.toUri.toString
    assert(uri.contains("%20"))
    assert(ClientUri.toSourceName(uri).contains(SourceName.PathName(path)))
  }

  test("ClientUri.FileUriWithNonAsciiNamesPath") {
    val path = absolute.resolve("prøj/Main.flix")
    val uri = path.toUri.toString
    assert(uri.contains("%C3%B8"))
    assert(ClientUri.toSourceName(uri).contains(SourceName.PathName(path)))
  }

  test("ClientUri.UntitledBufferKeepsUri") {
    val uri = "untitled:Untitled-1"
    assert(ClientUri.toSourceName(uri).contains(SourceName.UriName(new URI(uri))))
  }

  test("ClientUri.SchemelessStringNamesRelativePath") {
    // The compiler's own names, e.g. the test file and the library, have no scheme.
    val uri = "__TEST__.flix"
    assert(ClientUri.toSourceName(uri).contains(SourceName.PathName(Path.of(uri))))
  }

  test("ClientUri.Malformed") {
    assert(ClientUri.toSourceName("file:///a b/Main.flix").isEmpty)
  }

  test("ClientUri.RoundTrip.IsTheClientsOwnSpelling") {
    // Whatever the client sends comes back unchanged, including a Windows drive letter as VS Code
    // spells it, which Java would otherwise render as `file:///c:/…`.
    val uris = List(
      absolute.resolve("proj/Main.flix").toUri.toString,
      absolute.resolve("my proj/Main.flix").toUri.toString,
      "file:///c%3A/Users/me/Main.flix",
      "untitled:Untitled-1",
      "__TEST__.flix",
    )
    for (uri <- uris) {
      assert(roundTrip(uri).contains(uri), uri)
    }
  }

  test("ClientUri.Synthesized.AbsolutePathIsFileUri") {
    // A name the client never spelled, e.g. a file read from disk by the server.
    val path = absolute.resolve("never/spelled/by/a/client/Main.flix")
    assert(ClientUri.fromSourceName(SourceName.PathName(path)) == path.toUri.toString)
  }

  test("ClientUri.Synthesized.RelativePathIsItself") {
    // A file of the library.
    assert(ClientUri.fromSourceName(SourceName.PathName(Path.of("List.flix"))) == "List.flix")
  }

  test("ClientUri.Synthesized.PackageEntryIsItsName") {
    val name = SourceName.PackageEntry(Path.of("/deps/dep.fpkg"), "src/A.flix")
    assert(ClientUri.fromSourceName(name) == name.toString)
  }

}
