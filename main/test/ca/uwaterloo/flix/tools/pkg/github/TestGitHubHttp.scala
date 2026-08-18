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
package ca.uwaterloo.flix.tools.pkg.github

import ca.uwaterloo.flix.tools.pkg.github.GitHub.Project
import ca.uwaterloo.flix.tools.pkg.{PackageError, SemVer}
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import com.sun.net.httpserver.HttpServer
import org.scalatest.BeforeAndAfterAll
import org.scalatest.funsuite.AnyFunSuite

import java.io.IOException
import java.net.{InetSocketAddress, URI, URL}
import java.nio.charset.StandardCharsets

/**
  * Exercises [[GitHub]]'s HTTP handling deterministically: status codes, redirects and rate-limit
  * headers via a real local server, and the by-tag/listing split via a [[FakeTransport]]. Belongs
  * in the default `flix.test` pass rather than behind the network-gated `flix.testPackageManager`.
  */
class TestGitHubHttp extends AnyFunSuite with BeforeAndAfterAll {

  private var server: HttpServer = _
  private var port: Int = _

  override def beforeAll(): Unit = {
    server = HttpServer.create(new InetSocketAddress("localhost", 0), 0)
    port = server.getAddress.getPort

    def respond(
      path: String,
      status: Int,
      headers: Map[String, String] = Map.empty,
      body: String = ""
    ): Unit =
      server.createContext(path, exchange => {
        headers.foreach { case (k, v) => exchange.getResponseHeaders.add(k, v) }
        val bytes = body.getBytes(StandardCharsets.UTF_8)
        exchange.sendResponseHeaders(status, if (body.isEmpty) -1 else bytes.length.toLong)
        if (body.nonEmpty) exchange.getResponseBody.write(bytes)
        exchange.close()
      })

    respond("/ok", 200, body = "hello")
    respond("/redirect", 302, headers = Map("Location" -> s"http://localhost:$port/ok"))
    respond("/missing", 404)
    respond("/refused", 403, headers = Map(
      "Retry-After" -> "42",
      "X-RateLimit-Remaining" -> "0",
      "X-RateLimit-Reset" -> "1234567890"
    ))
    respond("/secondary-limit", 429)

    server.start()
  }

  override def afterAll(): Unit = server.stop(0)

  private def urlFor(path: String): URL = new URI(s"http://localhost:$port$path").toURL

  test("download succeeds, follows a redirect, and reports 404/403/429 distinctly") {
    implicit val transport: GitHub.Transport = GitHub.Transport.live
    for (path <- List("/ok", "/redirect")) {
      GitHub.download(urlFor(path)) match {
        case Ok(stream) =>
          try assertResult("hello")(new String(stream.readAllBytes(), StandardCharsets.UTF_8))
          finally stream.close()
        case Err(e) => fail(s"$path: ${e.getClass.getSimpleName}")
      }
    }
    GitHub.download(urlFor("/missing")) match {
      case Err(PackageError.DownloadFailed(_, 404)) => succeed
      case other => fail(other.toString)
    }
    GitHub.download(urlFor("/refused")) match {
      case Err(PackageError.DownloadRefused(_, 403, Some("42"), Some("1234567890"), Some("0"))) =>
        succeed
      case other => fail(other.toString)
    }
    GitHub.download(urlFor("/secondary-limit")) match {
      case Err(PackageError.DownloadRefused(_, 429, _, _, _)) => succeed
      case other => fail(other.toString)
    }
  }

  test("download reports an unreachable server distinctly from a refusal") {
    // A transport that never returns normally stands in for a dropped connection -- deterministic,
    // unlike the real socket timing a genuinely closed or firewalled port would need.
    implicit val unreachable: GitHub.Transport =
      GitHub.Transport(_ => throw new IOException("simulated: no route to host"))

    GitHub.download(new URI("http://example.invalid/x").toURL) match {
      case Err(PackageError.DownloadUnreachable(_, _)) => succeed
      case other => fail(other.toString)
    }
  }

  private val project = Project("flix", "museum")
  private val version = SemVer(1, 1, 0)
  private val listingUrl = "https://api.github.com/repos/flix/museum/releases"
  private val tagUrl = "https://api.github.com/repos/flix/museum/releases/tags/v1.1.0"
  private val museumFpkgJson =
    """{"tag_name":"v1.1.0","assets":[{"name":"museum.fpkg",""" +
      """"browser_download_url":"https://example.invalid/museum.fpkg"}]}"""

  test("getReleases reports 403/429 as refusals rather than JSON parse errors") {
    // Before this fix, a rate-limit error body -- itself valid but non-array JSON -- was fed
    // straight to the JSON parser and reported as a malformed release listing.
    for (status <- List(403, 429)) {
      val response = CannedResponse(
        status,
        headers = Map("X-RateLimit-Remaining" -> "0"),
        body = """{"message":"rate limited"}"""
      )
      implicit val transport: GitHub.Transport = FakeTransport(Map(listingUrl -> List(response)))
      GitHub.getReleases(project, apiKey = None) match {
        case Err(PackageError.DownloadRefused(_, `status`, _, _, Some("0"))) => succeed
        case other => fail(s"status $status: $other")
      }
    }
  }

  test("getReleaseByTag succeeds without reading the paginated listing," +
    " and reports 404/403 distinctly") {
    // Only the tag URL is ever scripted; a fallback to the listing makes FakeTransport throw.
    val ok200 = CannedResponse(200, body = museumFpkgJson)
    implicit val ok: GitHub.Transport = FakeTransport(Map(tagUrl -> List(ok200)))
    GitHub.getReleaseByTag(project, version, apiKey = None)(ok) match {
      case Ok(release) => assertResult(version)(release.version)
      case other => fail(other.toString)
    }

    val notFound404 = List(CannedResponse(404))
    implicit val notFound: GitHub.Transport = FakeTransport(Map(tagUrl -> notFound404))
    GitHub.getReleaseByTag(project, version, apiKey = None)(notFound) match {
      case Err(PackageError.VersionDoesNotExist(v, p)) =>
        assertResult(version)(v)
        assertResult(project)(p)
      case other => fail(other.toString)
    }

    // No X-RateLimit-Remaining: not confirmed as a rate limit, so GitHub's own message is kept
    // instead of being folded into a generic "this is usually a rate limit".
    val badCreds = CannedResponse(403, body = """{"message":"Bad credentials"}""")
    implicit val refused: GitHub.Transport = FakeTransport(Map(tagUrl -> List(badCreds)))
    GitHub.getReleaseByTag(project, version, apiKey = None)(refused) match {
      case Err(PackageError.RequestRefused(_, 403, Some("Bad credentials"))) => succeed
      case other => fail(other.toString)
    }
  }

  test("getReleaseByTag reports a truncated response body distinctly") {
    implicit val transport: GitHub.Transport = FakeTransport(Map(
      tagUrl -> List(CannedResponse(200, body = museumFpkgJson, failAfterBytes = Some(4)))
    ))
    GitHub.getReleaseByTag(project, version, apiKey = None) match {
      case Err(PackageError.ResponseBodyUnreadable(_, _)) => succeed
      case other => fail(other.toString)
    }
  }

  test("findReleaseAsset finds the single matching asset via the tag endpoint," +
    " not the listing") {
    val ok200 = CannedResponse(200, body = museumFpkgJson)
    implicit val transport: GitHub.Transport = FakeTransport(Map(tagUrl -> List(ok200)))
    GitHub.findReleaseAsset(project, version, "fpkg", apiKey = None) match {
      case Ok(asset) => assertResult("museum.fpkg")(asset.name)
      case other => fail(other.toString)
    }
  }

  test("getReleases follows Link: rel=\"next\" pagination across pages") {
    val page2 = "https://api.github.com/repos/flix/museum/releases?page=2"
    def releaseListJson(tag: String, assetName: String): String = {
      val url = s"https://example.invalid/$assetName"
      s"""[{"tag_name":"$tag","assets":[{"name":"$assetName","browser_download_url":"$url"}]}]"""
    }

    implicit val transport: GitHub.Transport = FakeTransport(Map(
      listingUrl -> List(CannedResponse(200,
        headers = Map("Link" -> s"""<$page2>; rel="next", <$page2>; rel="last""""),
        body = releaseListJson("v1.0.0", "museum-a.fpkg"))),
      page2 -> List(CannedResponse(200, body = releaseListJson("v1.1.0", "museum-b.fpkg")))
    ))

    GitHub.getReleases(project, apiKey = None) match {
      case Ok(releases) =>
        assertResult(List(SemVer(1, 0, 0), SemVer(1, 1, 0)))(releases.map(_.version))
      case other => fail(other.toString)
    }
  }
}
