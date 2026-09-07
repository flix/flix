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

// `GitHub.tryApiThenPublic` chooses the download route, so it is tested directly with stubbed
// attempts rather than against GitHub.

class TestGitHub extends AnyFunSuite {

  test("tryApiThenPublic.01: with an apiKey, the API is used without trying the public URL") {
    val result = GitHub.tryApiThenPublic(apiKey = Some("token"))(fail("must not try the public URL when an apiKey is available"))(key => s"api:$key")
    assertResult(expected = "api:token")(actual = result)
  }

  test("tryApiThenPublic.02: without an apiKey, the public URL is used without trying the API") {
    val result = GitHub.tryApiThenPublic(apiKey = None)("public")(_ => fail("must not try the API without an apiKey"))
    assertResult(expected = "public")(actual = result)
  }

  test("tryApiThenPublic.03: an API failure is not retried through the public URL") {
    assertThrows[RuntimeException] {
      GitHub.tryApiThenPublic(apiKey = Some("token"))(fail("must not try the public URL after an API failure"))(_ => throw new RuntimeException("refused"))
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
