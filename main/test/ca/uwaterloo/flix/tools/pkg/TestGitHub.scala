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
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.util.Formatter
import org.scalatest.DoNotDiscover
import org.scalatest.funsuite.AnyFunSuite

import java.net.{URI, URL}

@DoNotDiscover
class TestGitHub extends AnyFunSuite {

  test("getReleases.01") {
    // A project that exists is read as its releases.
    val project = GitHub.Project("flix", "museum-clerk")
    val releases = GitHub.getReleases(project, PkgTestUtils.gitHubToken).unsafeGet
    assert(releases.map(r => r.version).contains(SemVer(1, 1, 0)))
  }

  test("getReleases.02") {
    // A project that does not exist is said not to exist, rather than reported as a body that
    // could not be parsed.
    val project = GitHub.Project("flix", "no-such-package")
    GitHub.getReleases(project, PkgTestUtils.gitHubToken) match {
      case Ok(releases) => fail(s"Expected no such project, but got: $releases")
      case Err(e: PackageError.ProjectDoesNotExist) =>
        assert(e.project == project)
        // What GitHub answered is not what the user is told.
        assert(!e.message(Formatter.NoFormatter).contains("documentation_url"))
      case Err(e) => fail(s"Expected no such project, but got: ${e.message(Formatter.NoFormatter)}")
    }
  }

  test("getReleases.03") {
    // A token GitHub will not accept is reported as a rejected token, rather than as a response
    // nobody expected. The API answers 401 for one, where a request carrying none is let through.
    val project = GitHub.Project("flix", "museum-clerk")
    GitHub.getReleases(project, Some("not-a-token")) match {
      case Ok(releases) => fail(s"Expected a rejected token, but got: $releases")
      case Err(e: PackageError.TokenRejected) =>
        // What the token is is not what the user is told.
        assert(!e.message(Formatter.NoFormatter).contains("not-a-token"))
      case Err(e) => fail(s"Expected a rejected token, but got: ${e.message(Formatter.NoFormatter)}")
    }
  }

  test("downloadRefused.01") {
    // A refusal tells a client that carries no token how to set one, and does not tell a client
    // that already holds one to go and get a token it has.
    val url = mkUrl("https://api.github.com/repos/flix/museum-clerk/releases")
    val anonymous = PackageError.DownloadRefused(url, 403, None, authorized = false)
    val authorized = PackageError.DownloadRefused(url, 403, None, authorized = true)
    assert(anonymous.message(Formatter.NoFormatter).contains("GITHUB_TOKEN"))
    assert(!authorized.message(Formatter.NoFormatter).contains("GITHUB_TOKEN"))
  }

  test("getReleases.04") {
    // A repository whose releases are not tagged as versions of a package is read as holding
    // none of them, rather than throwing out of the listing and taking the build with it.
    // `microsoft/vscode` tags its releases `1.138.0`, without the leading `v`, which is a common
    // enough way to tag a release that a Flix package may well depend on a repository doing it.
    val project = GitHub.Project("microsoft", "vscode")
    val releases = GitHub.getReleases(project, PkgTestUtils.gitHubToken).unsafeGet
    assert(releases.isEmpty)
  }

  test("downloadReleaseAsset.01") {
    // A release asset is downloaded with whatever token is held, and a release address ignores a
    // token it does not accept rather than refusing the request. A token that has gone stale in
    // the environment therefore does not break a build that would have worked without one.
    val project = GitHub.Project("flix", "museum-clerk")
    val stream = GitHub.downloadReleaseAsset(project, SemVer(1, 1, 0), "flix.toml", Some("not-a-token")).unsafeGet
    try {
      assert(stream.readAllBytes().nonEmpty)
    } finally {
      stream.close()
    }
  }

  test("mayReceiveToken.01") {
    // The hosts a token is for: the API, the addresses releases are downloaded from, and the
    // one assets are uploaded to.
    assert(GitHub.mayReceiveToken(mkUrl("https://api.github.com/repos/flix/museum-clerk/releases")))
    assert(GitHub.mayReceiveToken(mkUrl("https://github.com/flix/museum-clerk/releases/download/v1.1.0/package.fpkg")))
    assert(GitHub.mayReceiveToken(mkUrl("https://uploads.github.com/repos/flix/museum-clerk/releases/1/assets?name=package.fpkg")))
  }

  test("mayReceiveToken.02") {
    // A host that is not GitHub's, including the storage a release asset is served from, a host
    // that merely ends in one of GitHub's, and one that merely mentions it.
    assert(!GitHub.mayReceiveToken(mkUrl("https://objects.githubusercontent.com/github-production-release-asset/1/2")))
    assert(!GitHub.mayReceiveToken(mkUrl("https://github.com.example.com/flix/museum-clerk")))
    assert(!GitHub.mayReceiveToken(mkUrl("https://example.com/github.com/flix/museum-clerk")))
  }

  test("mayReceiveToken.03") {
    // A token that is sent in the clear is a token that has been given away.
    assert(!GitHub.mayReceiveToken(mkUrl("http://github.com/flix/museum-clerk/releases/download/v1.1.0/package.fpkg")))
  }

  test("mayReceiveToken.04") {
    // A host is the same host however it is written.
    assert(GitHub.mayReceiveToken(mkUrl("https://API.GitHub.COM/repos/flix/museum-clerk/releases")))
  }

  test("newRequest.01") {
    // A request to GitHub carries the token.
    val url = mkUrl("https://api.github.com/repos/flix/museum-clerk/releases")
    val req = GitHub.newRequest(url, Some("a-token")).GET().build()
    assert(req.headers().firstValue("Authorization").orElse("") == "Bearer a-token")
  }

  test("newRequest.02") {
    // A request to anywhere else carries none of it, however the token was come by. A jar is
    // downloaded from wherever the manifest that declares it says, which is not GitHub's to
    // choose and may be anyone's to write.
    val url = mkUrl("https://example.com/museum-clerk.jar")
    val req = GitHub.newRequest(url, Some("a-token")).GET().build()
    assert(req.headers().firstValue("Authorization").isEmpty)
  }

  test("newRequest.03") {
    // A request made without a token is made without authorization, rather than with an empty one.
    val url = mkUrl("https://api.github.com/repos/flix/museum-clerk/releases")
    val req = GitHub.newRequest(url, None).GET().build()
    assert(req.headers().firstValue("Authorization").isEmpty)
  }

  /**
    * Returns `s` as a URL.
    */
  private def mkUrl(s: String): URL = new URI(s).toURL

}
