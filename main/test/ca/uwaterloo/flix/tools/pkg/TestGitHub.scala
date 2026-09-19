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

}
