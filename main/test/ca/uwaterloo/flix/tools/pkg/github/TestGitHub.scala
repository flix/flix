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

import org.json4s.JsonDSL.*
import org.json4s.JValue
import org.scalatest.funsuite.AnyFunSuite

import java.io.FileNotFoundException

// `GitHub.tryPublicThenApi` decides whether a private repo's release assets can be downloaded at
// all, so it is tested directly, with stubbed attempts, rather than against GitHub.

class TestGitHub extends AnyFunSuite {

  test("tryPublicThenApi.01: a public hit is used, without trying the API") {
    val result = GitHub.tryPublicThenApi(apiKey = Some("token"))("public")(_ => fail("must not try the API after a public hit"))
    assertResult(expected = "public")(actual = result)
  }

  test("tryPublicThenApi.02: without an apiKey, a 404 is not retried") {
    assertThrows[FileNotFoundException] {
      GitHub.tryPublicThenApi(apiKey = None)(throw new FileNotFoundException())(_ => fail("must not try the API without an apiKey"))
    }
  }

  test("tryPublicThenApi.03: with an apiKey, a 404 falls back to the API attempt, given that key") {
    val result = GitHub.tryPublicThenApi(apiKey = Some("token"))(throw new FileNotFoundException())(key => s"api:$key")
    assertResult(expected = "api:token")(actual = result)
  }

  test("tryPublicThenApi.04: a failure other than a 404 is not retried, even with an apiKey") {
    assertThrows[RuntimeException] {
      GitHub.tryPublicThenApi(apiKey = Some("token"))(throw new RuntimeException("refused"))(_ => fail("must not try the API after a non-404 failure"))
    }
  }

  test("parseAsset.01: url and apiUrl are read from different JSON fields") {
    val json: JValue =
      ("name" -> "flix.toml") ~
        ("browser_download_url" -> "https://github.com/wstein/pr13165-package/releases/download/v0.1.1/flix.toml") ~
        ("url" -> "https://api.github.com/repos/wstein/pr13165-package/releases/assets/1")

    val asset = GitHub.parseAsset(json)

    assertResult(expected = "flix.toml")(actual = asset.name)
    assertResult(expected = "https://github.com/wstein/pr13165-package/releases/download/v0.1.1/flix.toml")(actual = asset.url.toString)
    assertResult(expected = "https://api.github.com/repos/wstein/pr13165-package/releases/assets/1")(actual = asset.apiUrl.toString)
  }

}
