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
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.util.{Result, Sha256}
import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.{Path, Paths}

class TestEffectLockfile extends AnyFunSuite {

  private val path: Path = Paths.get("effects.lock")

  private val clerk: PackageId = PackageId.mkPackageId("github:flix/museum-clerk").get

  private val museum: PackageId = PackageId.mkPackageId("github:flix/museum").get

  private val h1: Sha256 = Sha256.parse("sha256:" + "1" * 64).get

  private val h2: Sha256 = Sha256.parse("sha256:" + "2" * 64).get

  private val h3: Sha256 = Sha256.parse("sha256:" + "3" * 64).get

  //
  // Format.
  //

  test("format.01") {
    // A lock file that locks nothing still says what it is.
    val actual = EffectLockfile.format(EffectLockfile(Map.empty))
    assert(actual == "[lock]\nversion = 1\n")
  }

  test("format.02") {
    val lockfile = EffectLockfile(Map(clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1), Map.empty)))
    val expected =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum-clerk".defs]
        |"Museum.Clerk.sell" = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin
    assert(EffectLockfile.format(lockfile) == expected)
  }

  test("format.03") {
    val lockfile = EffectLockfile(Map(clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1), Map("Museum.Clerk.Sellable.price" -> h2))))
    val expected =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum-clerk".defs]
        |"Museum.Clerk.sell" = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |
        |[packages."github:flix/museum-clerk".sigs]
        |"Museum.Clerk.Sellable.price" = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        |""".stripMargin
    assert(EffectLockfile.format(lockfile) == expected)
  }

  test("format.04") {
    // A table that locks nothing is left out.
    val lockfile = EffectLockfile(Map(clerk -> LockedPackage(Map.empty, Map("Museum.Clerk.Sellable.price" -> h2))))
    assert(!EffectLockfile.format(lockfile).contains("defs"))
    assert(EffectLockfile.format(lockfile).contains("sigs"))
  }

  test("format.05") {
    // Packages are written in order of identifier, and their symbols in order of name.
    val lockfile = EffectLockfile(Map(
      clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1, "Museum.Clerk.buy" -> h2), Map.empty),
      museum -> LockedPackage(Map("Museum.open" -> h3), Map.empty)
    ))
    val expected =
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum".defs]
        |"Museum.open" = "sha256:3333333333333333333333333333333333333333333333333333333333333333"
        |
        |[packages."github:flix/museum-clerk".defs]
        |"Museum.Clerk.buy" = "sha256:2222222222222222222222222222222222222222222222222222222222222222"
        |"Museum.Clerk.sell" = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin
    assert(EffectLockfile.format(lockfile) == expected)
  }

  test("format.06") {
    // The same lock file formats the same however its maps were built.
    val one = EffectLockfile(Map(
      museum -> LockedPackage(Map("Museum.open" -> h3), Map.empty),
      clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1, "Museum.Clerk.buy" -> h2), Map.empty)
    ))
    val other = EffectLockfile(Map(
      clerk -> LockedPackage(Map("Museum.Clerk.buy" -> h2, "Museum.Clerk.sell" -> h1), Map.empty),
      museum -> LockedPackage(Map("Museum.open" -> h3), Map.empty)
    ))
    assert(EffectLockfile.format(one) == EffectLockfile.format(other))
  }

  test("format.07") {
    // A lock file never holds a carriage return, whichever platform wrote it.
    val lockfile = EffectLockfile(Map(clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1), Map("Museum.Clerk.Sellable.price" -> h2))))
    assert(!EffectLockfile.format(lockfile).contains("\r"))
  }

  //
  // Round trip.
  //

  test("roundtrip.01") {
    assertRoundTrip(EffectLockfile(Map.empty))
  }

  test("roundtrip.02") {
    assertRoundTrip(EffectLockfile(Map(clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1), Map.empty))))
  }

  test("roundtrip.03") {
    assertRoundTrip(EffectLockfile(Map(clerk -> LockedPackage(Map.empty, Map("Museum.Clerk.Sellable.price" -> h2)))))
  }

  test("roundtrip.04") {
    assertRoundTrip(EffectLockfile(Map(
      clerk -> LockedPackage(Map("Museum.Clerk.sell" -> h1, "Museum.Clerk.buy" -> h2), Map("Museum.Clerk.Sellable.price" -> h3)),
      museum -> LockedPackage(Map("Museum.open" -> h3), Map.empty)
    )))
  }

  test("roundtrip.05") {
    // A symbol at the top level of a package holds no dot, and one nested deeply holds several.
    assertRoundTrip(EffectLockfile(Map(clerk -> LockedPackage(Map("sell" -> h1, "A.B.C.D.E.f" -> h2), Map.empty))))
  }

  //
  // Parse.
  //

  test("parse.01") {
    // A lock file with no packages table locks no package.
    val actual = EffectLockfileParser.parse("[lock]\nversion = 1\n", path)
    assert(actual == Result.Ok(EffectLockfile(Map.empty)))
  }

  test("parse.02") {
    assertError[EffectLockError.MissingRequiredProperty]("[packages]\n")
  }

  test("parse.03") {
    assertError[EffectLockError.UnsupportedLockVersion]("[lock]\nversion = 2\n")
  }

  test("parse.04") {
    assertError[EffectLockError.PropertyHasWrongType]("[lock]\nversion = \"1\"\n")
  }

  test("parse.05") {
    assertError[EffectLockError.IllegalTableFound]("[lock]\nversion = 1\n\n[wombat]\n")
  }

  test("parse.06") {
    assertError[EffectLockError.IllegalLockKeyFound]("[lock]\nversion = 1\nwombat = 2\n")
  }

  test("parse.07") {
    assertError[EffectLockError.IllegalPackageTableFound](
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum-clerk".wombat]
        |"Museum.Clerk.sell" = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin)
  }

  test("parse.08") {
    assertError[EffectLockError.IllegalHash](
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum-clerk".defs]
        |"Museum.Clerk.sell" = "sha256:cafebabe"
        |""".stripMargin)
  }

  test("parse.09") {
    // A hash of the right shape but of another algorithm is not one Flix wrote.
    assertError[EffectLockError.IllegalHash](
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum-clerk".defs]
        |"Museum.Clerk.sell" = "sha1:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin)
  }

  test("parse.10") {
    assertError[EffectLockError.LockParseError]("this is not toml")
  }

  test("parse.11") {
    assertError[EffectLockError.PropertyHasWrongType](
      """[lock]
        |version = 1
        |
        |[packages."github:flix/museum-clerk".defs]
        |"Museum.Clerk.sell" = 42
        |""".stripMargin)
  }

  test("parse.12") {
    // A key that names no package is dropped, so that the file can be written back without it.
    val input =
      """[lock]
        |version = 1
        |
        |[packages."not a package".defs]
        |"Museum.Clerk.sell" = "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        |""".stripMargin
    assert(EffectLockfileParser.parse(input, path) == Result.Ok(EffectLockfile(Map.empty)))
  }

  /**
    * Asserts that formatting `lockfile` and parsing the result yields `lockfile`.
    */
  private def assertRoundTrip(lockfile: EffectLockfile): Unit = {
    val formatted = EffectLockfile.format(lockfile)
    EffectLockfileParser.parse(formatted, path) match {
      case Result.Ok(actual) => assert(actual == lockfile, s"formatted as:\n$formatted")
      case Result.Err(e) => fail(s"could not parse:\n$formatted")
    }
  }

  /**
    * Asserts that parsing `input` fails with an error of type `T`.
    */
  private def assertError[T](input: String)(implicit tag: scala.reflect.ClassTag[T]): Unit = {
    EffectLockfileParser.parse(input, path) match {
      case Result.Ok(lockfile) => fail(s"expected ${tag.runtimeClass.getSimpleName}, but parsed $lockfile")
      case Result.Err(e) => assert(tag.runtimeClass.isInstance(e), s"expected ${tag.runtimeClass.getSimpleName}, but found $e")
    }
  }

}
