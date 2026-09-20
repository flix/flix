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
package ca.uwaterloo.flix.api

import ca.uwaterloo.flix.api.effectlock.EffectLockError
import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.tools.pkg.{LockError, ManifestError, PackageError, ReleaseError}
import ca.uwaterloo.flix.util.Formatter
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Path, Paths}

/**
  * The tests of the messages that the package manager prints when a command fails.
  *
  * A message does not end with a line separator: the caller that prints one adds it, and a
  * message that ends with one of its own leaves a blank line behind. The messages are written as
  * one string each, where the separator is easy to add back without anything noticing, so each
  * kind of message is held to the rule here.
  */
class TestBootstrapError extends AnyFunSuite {

  private val path: Path = Paths.get("flix.toml")

  private val f: Formatter = Formatter.NoFormatter

  test("bootstrap-error.01") {
    assertNoTrailingSeparator(BootstrapError.PackageNotInstalled(PackageId.mkPackageId("github:flix/museum").get).message(f))
  }

  test("bootstrap-error.02") {
    assertNoTrailingSeparator(BootstrapError.NoProject(path).message(f))
  }

  test("effect-lock-error.01") {
    assertNoTrailingSeparator(EffectLockError.UnsupportedLockVersion(path, 2L).message(f))
  }

  test("lock-error.01") {
    assertNoTrailingSeparator(LockError.UnsupportedLockVersion(path, 2L).message(f))
  }

  test("manifest-error.01") {
    assertNoTrailingSeparator(ManifestError.FlixVersionHasWrongLength(path, "1.2").message(f))
  }

  test("package-error.01") {
    assertNoTrailingSeparator(PackageError.InvalidProjectName("not a project").message(f))
  }

  test("release-error.01") {
    assertNoTrailingSeparator(ReleaseError.MissingManifest.message(f))
  }

  /**
    * Asserts that `message` does not end with a line separator, in any of its spellings.
    */
  private def assertNoTrailingSeparator(message: String): Unit = {
    assert(!message.endsWith("\n"), s"message ends with a line separator: '$message'")
    assert(!message.endsWith("\r"), s"message ends with a line separator: '$message'")
    assert(!message.endsWith(System.lineSeparator()), s"message ends with a line separator: '$message'")
  }

}
