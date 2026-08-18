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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.tools.pkg.github.{CannedResponse, FakeTransport, GitHub}
import ca.uwaterloo.flix.util.Formatter
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import org.scalatest.funsuite.AnyFunSuite

import java.io.{OutputStream, PrintStream}
import java.nio.file.Files
import scala.util.Using
import scala.jdk.CollectionConverters.*

/**
  * Exercises [[FlixPackageManager.install]] and [[FlixPackageManager.openAsset]] against a
  * [[FakeTransport]]: the candidate-then-fallback search, and the atomic-write guarantee that a
  * failed download cannot be cached as if it had succeeded. Deterministic and quota-free, so it
  * belongs in the default `flix.test` pass rather than behind the network-gated
  * `flix.testPackageManager`.
  */
class TestFlixPackageManagerInstall extends AnyFunSuite {

  private implicit val formatter: Formatter = Formatter.NoFormatter
  private implicit val out: PrintStream = new PrintStream(OutputStream.nullOutputStream())

  private val version = SemVer(1, 0, 0)
  private val tagUrl = "https://api.github.com/repos/flix/museum/releases/tags/v1.0.0"

  private def assetUrl(name: String): String =
    s"https://github.com/flix/museum/releases/download/v1.0.0/$name"

  test("install succeeds from the first candidate that hits, without reading the listing") {
    // The first case never queries "museum-clerk.fpkg"; the second 404s on "museum.fpkg" first and
    // falls through to it. Neither queries the listing -- FakeTransport would throw if they did.
    val candidates = List("museum.fpkg", "museum-clerk.fpkg")
    val scripts = List(
      Map(assetUrl("museum.fpkg") -> List(CannedResponse(200, body = "package-bytes"))),
      Map(
        assetUrl("museum.fpkg") -> List(CannedResponse(404)),
        assetUrl("museum-clerk.fpkg") -> List(CannedResponse(200, body = "package-bytes"))
      )
    )
    for (script <- scripts) {
      implicit val transport: GitHub.Transport = FakeTransport(script)
      val root = Files.createTempDirectory("install-test")
      val result = FlixPackageManager
        .install("flix/museum", version, "fpkg", candidates, root, apiKey = None)
      result match {
        case Ok(path) => assertResult("package-bytes")(Files.readString(path))
        case Err(e) => fail(e.message(formatter))
      }
    }
  }

  test("install falls back to the listing when every candidate 404s") {
    val assetName = "published-under-a-different-name.fpkg"
    val listingBody =
      s"""{"tag_name":"v1.0.0","assets":[{"name":"$assetName",""" +
        """"browser_download_url":"https://example.invalid/asset.fpkg"}]}"""
    implicit val transport: GitHub.Transport = FakeTransport(Map(
      assetUrl("museum.fpkg") -> List(CannedResponse(404)),
      assetUrl("museum-clerk.fpkg") -> List(CannedResponse(404)),
      tagUrl -> List(CannedResponse(200, body = listingBody)),
      "https://example.invalid/asset.fpkg" -> List(CannedResponse(200, body = "package-bytes"))
    ))
    val root = Files.createTempDirectory("install-test")
    val candidates = List("museum.fpkg", "museum-clerk.fpkg")

    val result = FlixPackageManager
      .install("flix/museum", version, "fpkg", candidates, root, apiKey = None)
    result match {
      case Ok(path) => assertResult("package-bytes")(Files.readString(path))
      case Err(e) => fail(e.message(formatter))
    }
  }

  test("a download that fails partway through is not cached as a successful install") {
    val response = CannedResponse(200, body = "a-lot-of-package-bytes", failAfterBytes = Some(4))
    val script = Map(assetUrl("museum.fpkg") -> List(response))
    implicit val transport: GitHub.Transport = FakeTransport(script)
    val root = Files.createTempDirectory("install-test")

    val candidates = List("museum.fpkg")
    val result = FlixPackageManager
      .install("flix/museum", version, "fpkg", candidates, root, apiKey = None)
    val msg = s"expected the failed download to be reported as an error, got $result"
    assert(result.toOption.isEmpty, msg)

    val dirPath = Bootstrap.getLibraryDirectory(root)
      .resolve("github").resolve("flix").resolve("museum").resolve(version.toString)
    val filesLeftBehind = Using(Files.list(dirPath))(_.iterator().asScala.toList).get
    assertResult(List.empty)(filesLeftBehind)
  }
}
