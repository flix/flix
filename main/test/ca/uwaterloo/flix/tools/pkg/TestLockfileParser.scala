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

import ca.uwaterloo.flix.util.Result.{Err, Ok}
import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.util.Sha256
import org.scalatest.funsuite.AnyFunSuite

import java.nio.charset.StandardCharsets
import java.nio.file.Path

class TestLockfileParser extends AnyFunSuite {

  /**
    * The path a lock file parsed from a string is reported as coming from.
    */
  private val LockPath: Path = Path.of("packages.lock")

  /** Returns `s` as a package identifier. */
  private def pkg(s: String): PackageId = PackageId.mkPackageId(s).get

  /**
    * Returns the digest of `s`, for use as a stand-in for the digest of a real file.
    */
  private def digestOf(s: String): Sha256 = Sha256.ofBytes(s.getBytes(StandardCharsets.UTF_8))

  /**
    * Returns the lock file that `s` denotes, and fails the test if `s` denotes none.
    */
  private def parse(s: String): Lockfile = LockfileParser.parse(s, LockPath) match {
    case Ok(lockfile) => lockfile
    case Err(e) => fail(s"Expected a lock file, but got: $e")
  }

  /**
    * Returns the error that parsing `s` produces, and fails the test if it produces none.
    */
  private def parseErr(s: String): LockError = LockfileParser.parse(s, LockPath) match {
    case Ok(lockfile) => fail(s"Expected an error, but got: $lockfile")
    case Err(e) => e
  }

  /**
    * A lock file that records two packages, each at the one version it is required at.
    */
  private val TwoPackages: Lockfile = Lockfile(Map(
    (pkg("github:flix/museum"), SemVer(1, 2, 3)) -> LockEntry(digestOf("museum.toml"), Some(digestOf("museum.fpkg"))),
    (pkg("github:flix/museum-clerk"), SemVer(0, 4, 0)) -> LockEntry(digestOf("clerk.toml"), Some(digestOf("clerk.fpkg")))
  ))

  /**
    * A lock file that records a package at two versions, one of which has been downloaded.
    */
  private val TwoVersions: Lockfile = Lockfile(Map(
    (pkg("github:flix/museum-clerk"), SemVer(0, 4, 0)) -> LockEntry(digestOf("clerk-0.4.0.toml"), None),
    (pkg("github:flix/museum-clerk"), SemVer(0, 4, 1)) -> LockEntry(digestOf("clerk-0.4.1.toml"), Some(digestOf("clerk-0.4.1.fpkg")))
  ))

  test("parse.empty.01") {
    // A project with no Flix dependencies locks no packages.
    assert(parse("[lock]\nversion = 1\n") == Lockfile(Map.empty))
  }

  test("parse.empty.02") {
    // An empty `packages` table is the same as no `packages` table.
    assert(parse("[lock]\nversion = 1\n\n[packages]\n") == Lockfile(Map.empty))
  }

  test("parse.01") {
    val lockfile = parse(
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin)

    val expected = LockEntry(Sha256("0" * 64), Some(Sha256("1" * 64)))
    assert(lockfile.packages == Map((pkg("github:flix/museum"), SemVer(1, 2, 3)) -> expected))
  }

  test("parse.02") {
    // The identifier keeps the `:` and the `/` it is written with, and the version its dots.
    val lockfile = parse(Lockfile.format(TwoPackages))
    assert(lockfile.packages.keySet == Set(
      (pkg("github:flix/museum"), SemVer(1, 2, 3)),
      (pkg("github:flix/museum-clerk"), SemVer(0, 4, 0))
    ))
  }

  test("parse.03") {
    // A version whose package has not been downloaded records no fpkg.
    val lockfile = parse(
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |""".stripMargin)

    assert(lockfile.packages == Map((pkg("github:flix/museum"), SemVer(1, 2, 3)) -> LockEntry(Sha256("0" * 64), None)))
  }

  test("parse.04") {
    // A package that is required at two versions has an entry for each.
    val lockfile = parse(
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |
        |[packages."github:flix/museum"."1.3.0"]
        |toml    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |fpkg    = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        |""".stripMargin)

    assert(lockfile.packages == Map(
      (pkg("github:flix/museum"), SemVer(1, 2, 3)) -> LockEntry(Sha256("0" * 64), None),
      (pkg("github:flix/museum"), SemVer(1, 3, 0)) -> LockEntry(Sha256("1" * 64), Some(Sha256("2" * 64)))
    ))
  }

  test("parse.05") {
    // Two versions of a package may each record an fpkg: an entry says what a file is, and what
    // is true of one version says nothing of another.
    val lockfile = parse(
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |
        |[packages."github:flix/museum"."1.3.0"]
        |toml    = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        |fpkg    = "sha256:3333333333333333333333333333333333333333333333333333333333333333"
        |""".stripMargin)

    assert(lockfile.packages == Map(
      (pkg("github:flix/museum"), SemVer(1, 2, 3)) -> LockEntry(Sha256("0" * 64), Some(Sha256("1" * 64))),
      (pkg("github:flix/museum"), SemVer(1, 3, 0)) -> LockEntry(Sha256("2" * 64), Some(Sha256("3" * 64)))
    ))
  }

  test("format.01") {
    assert(Lockfile.format(Lockfile(Map.empty)) == "[lock]\nversion = 1\n")
  }

  test("format.02") {
    val lockfile = Lockfile(Map(
      (pkg("github:flix/museum"), SemVer(1, 2, 3)) -> LockEntry(Sha256("a" * 64), Some(Sha256("b" * 64)))
    ))
    val expected =
      "[lock]\n" +
        "version = 1\n" +
        "\n" +
        "[packages.\"github:flix/museum\".\"1.2.3\"]\n" +
        s"toml    = \"sha256:${"a" * 64}\"\n" +
        s"fpkg    = \"sha256:${"b" * 64}\"\n"
    assert(Lockfile.format(lockfile) == expected)
  }

  test("format.03") {
    // Entries are written in order of identifier, whatever order the map holds them in.
    val forwards = Lockfile(Map(
      (pkg("github:flix/a"), SemVer(1, 0, 0)) -> LockEntry(Sha256("a" * 64), Some(Sha256("a" * 64))),
      (pkg("github:flix/b"), SemVer(1, 0, 0)) -> LockEntry(Sha256("b" * 64), Some(Sha256("b" * 64)))
    ))
    val backwards = Lockfile(Map(
      (pkg("github:flix/b"), SemVer(1, 0, 0)) -> LockEntry(Sha256("b" * 64), Some(Sha256("b" * 64))),
      (pkg("github:flix/a"), SemVer(1, 0, 0)) -> LockEntry(Sha256("a" * 64), Some(Sha256("a" * 64)))
    ))
    assert(Lockfile.format(forwards) == Lockfile.format(backwards))
    assert(Lockfile.format(forwards).indexOf("github:flix/a") < Lockfile.format(forwards).indexOf("github:flix/b"))
  }

  test("format.04") {
    // A lock file is committed and rewritten on every build, so its line endings must not depend
    // on the platform that wrote it.
    assert(!Lockfile.format(TwoPackages).contains("\r"))
  }

  test("format.05") {
    // An entry that records no fpkg writes no fpkg.
    val expected =
      "[lock]\n" +
        "version = 1\n" +
        "\n" +
        "[packages.\"github:flix/museum-clerk\".\"0.4.0\"]\n" +
        s"toml    = \"${digestOf("clerk-0.4.0.toml")}\"\n" +
        "\n" +
        "[packages.\"github:flix/museum-clerk\".\"0.4.1\"]\n" +
        s"toml    = \"${digestOf("clerk-0.4.1.toml")}\"\n" +
        s"fpkg    = \"${digestOf("clerk-0.4.1.fpkg")}\"\n"
    assert(Lockfile.format(TwoVersions) == expected)
  }

  test("format.06") {
    // The versions of a package are written in order of version, not of how they are spelled.
    val lockfile = Lockfile(Map(
      (pkg("github:flix/museum"), SemVer(1, 10, 0)) -> LockEntry(Sha256("a" * 64), None),
      (pkg("github:flix/museum"), SemVer(1, 9, 0)) -> LockEntry(Sha256("b" * 64), None)
    ))
    assert(Lockfile.format(lockfile).indexOf("\"1.9.0\"") < Lockfile.format(lockfile).indexOf("\"1.10.0\""))
  }

  test("roundtrip.01") {
    assert(parse(Lockfile.format(TwoPackages)) == TwoPackages)
  }

  test("roundtrip.02") {
    assert(parse(Lockfile.format(Lockfile(Map.empty))) == Lockfile(Map.empty))
  }

  test("roundtrip.03") {
    // Formatting is stable: a lock file that is read and written again is unchanged.
    val once = Lockfile.format(TwoPackages)
    assert(Lockfile.format(parse(once)) == once)
  }

  test("roundtrip.04") {
    assert(parse(Lockfile.format(TwoVersions)) == TwoVersions)
  }

  test("version.01") {
    // A lock file must say which version of the format it is written in.
    assert(parseErr("[packages]\n").isInstanceOf[LockError.MissingRequiredProperty])
  }

  test("version.02") {
    assert(parseErr("[lock]\n").isInstanceOf[LockError.MissingRequiredProperty])
  }

  test("version.03") {
    // A version this Flix does not understand is reported, not guessed at.
    assert(parseErr("[lock]\nversion = 2\n").isInstanceOf[LockError.UnsupportedLockVersion])
  }

  test("version.04") {
    assert(parseErr("[lock]\nversion = \"1\"\n").isInstanceOf[LockError.PropertyHasWrongType])
  }

  test("keys.01") {
    // A table Flix never writes.
    assert(parseErr("[lock]\nversion = 1\n\n[dependencies]\n").isInstanceOf[LockError.IllegalTableFound])
  }

  test("keys.02") {
    // A key in `lock` that Flix never writes.
    assert(parseErr("[lock]\nversion = 1\nnote = \"hello\"\n").isInstanceOf[LockError.IllegalLockKeyFound])
  }

  test("keys.03") {
    // A key in an entry that Flix never writes.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |jar     = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.IllegalPackageKeyFound])
  }

  test("entry.01") {
    // An entry with no toml digest.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |fpkg = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.MissingRequiredProperty])
  }

  test("entry.02") {
    // A version that is a string rather than a table.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"]
        |"1.2.3" = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.PropertyHasWrongType])
  }

  test("entry.03") {
    // A version that is not a semantic version.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.IllegalVersion])
  }

  test("entry.04") {
    // A digest with no algorithm.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.IllegalDigest])
  }

  test("entry.05") {
    // A digest that is too short.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.IllegalDigest])
  }

  test("entry.06") {
    // A package entry that is a string rather than a table.
    val s =
      """[lock]
        |version = 1
        |
        |[packages]
        |"github:flix/museum" = "1.2.3"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.PropertyHasWrongType])
  }

  test("toml.01") {
    // Not toml at all.
    assert(parseErr("this is not toml").isInstanceOf[LockError.LockParseError])
  }

  test("toml.02") {
    // The same package at the same version twice, which toml itself forbids.
    val s =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:0000000000000000000000000000000000000000000000000000000000000000"
        |fpkg    = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |
        |[packages."github:flix/museum"."1.2.3"]
        |toml    = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        |fpkg    = "sha256:3333333333333333333333333333333333333333333333333333333333333333"
        |""".stripMargin
    assert(parseErr(s).isInstanceOf[LockError.LockParseError])
  }

}
