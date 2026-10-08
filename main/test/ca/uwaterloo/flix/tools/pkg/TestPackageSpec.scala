/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.shared.{PackageId, Repository}
import org.scalatest.funsuite.AnyFunSuite

class TestPackageSpec extends AnyFunSuite {

  private val Clerk: PackageId = PackageId(Repository.GitHub, "flix", "museum-clerk")

  test("Ok.unqualified") {
    val spec = PackageSpec.mkPackageSpec("flix/museum-clerk").get
    assert(spec.id == Clerk)
    assert(spec.version.isEmpty)
  }

  test("Ok.qualified") {
    val spec = PackageSpec.mkPackageSpec("github:flix/museum-clerk").get
    assert(spec.id == Clerk)
    assert(spec.version.isEmpty)
  }

  test("Ok.version") {
    val spec = PackageSpec.mkPackageSpec("flix/museum-clerk@1.2.3").get
    assert(spec.id == Clerk)
    assert(spec.version.contains(SemVer(1, 2, 3)))
  }

  test("Ok.qualified.version") {
    val spec = PackageSpec.mkPackageSpec("github:flix/museum-clerk@1.2.3").get
    assert(spec.id == Clerk)
    assert(spec.version.contains(SemVer(1, 2, 3)))
  }

  test("Err.no-owner") {
    assert(PackageSpec.mkPackageSpec("museum-clerk").isEmpty)
  }

  test("Err.unknown-host") {
    assert(PackageSpec.mkPackageSpec("gitlab:flix/museum-clerk").isEmpty)
  }

  test("Err.illegal-name") {
    assert(PackageSpec.mkPackageSpec("flix/museum.clerk").isEmpty)
  }

  test("Err.empty-version") {
    assert(PackageSpec.mkPackageSpec("flix/museum-clerk@").isEmpty)
  }

  test("Err.illegal-version") {
    // A version that is written must be one: a specification that asks for 'latest' does not
    // silently fall back to the newest release.
    assert(PackageSpec.mkPackageSpec("flix/museum-clerk@latest").isEmpty)
  }

  test("Err.partial-version") {
    assert(PackageSpec.mkPackageSpec("flix/museum-clerk@1.2").isEmpty)
  }

  test("Err.empty") {
    assert(PackageSpec.mkPackageSpec("").isEmpty)
  }

}
