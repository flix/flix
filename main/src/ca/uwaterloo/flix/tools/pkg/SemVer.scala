/*
 * Copyright 2023 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Version

object SemVer {

  /**
    * An ordering on Semantic Versions
    */
  implicit def semVerOrdering: Ordering[SemVer] =
    Ordering.by((_: SemVer).major)
      .orElseBy(_.minor)
      .orElseBy(_.patch)

  /**
    * Returns the given Flix version `v` as a semantic version.
    */
  def ofVersion(v: Version): SemVer = SemVer(v.major, v.minor, v.revision)

  def ofString(input: String): Option[SemVer] = {
    val numPattern = raw"0|[1-9]\d*".r
    val pattern = raw"^($numPattern)\.($numPattern)\.($numPattern)$$".r
    input match {
      case pattern(major, minor, patch) =>
        Some(SemVer(major.toInt, minor.toInt, patch.toInt))
      case _ => None
    }
  }
}

/**
  * A semantic version number.
  */
case class SemVer(major: Int, minor: Int, patch: Int) {
  override def toString: String = s"$major.$minor.$patch"

  /**
    * Of the given `versions`, get the newest version which is a major update, if one exists.
    *
    * Example:
    *   - this = 1.2.0
    *   - versions = 1.2.0, 1.2.1, 1.3.0, 2.0.0
    *   - -> 2.0.0
    */
  def majorUpdate(versions: List[SemVer]): Option[SemVer] =
    versions.filter(v =>
      v.major > major
    ).maxOption

  /**
    * Of the given `versions`, get the newest version which is a minor update, if one exists.
    *
    * Example:
    *   - this = 1.2.0
    *   - versions = 1.2.0, 1.2.1, 1.3.0, 2.0.0
    *   - -> 1.3.0
    */
  def minorUpdate(versions: List[SemVer]): Option[SemVer] =
    versions.filter(v =>
      v.major == major && v.minor > minor
    ).maxOption

  /**
    * Of the given `versions`, get the newest version which is a patch update, if one exists.
    *
    * Example 1:
    *   - this = 1.2.0
    *   - versions = 1.2.0, 1.2.1, 1.3.0, 2.0.0
    *   - -> 1.2.1
    *
    * Example 2:
    *   - this = 1.2
    *   - versions = 1.2, 1.2.1
    *   - -> None
    *
    * Example 3:
    *   - this = 1.2.0
    *   - versions = 1.2.0, 1.2
    *   - -> None
    */
  def patchUpdate(versions: List[SemVer]): Option[SemVer] =
    versions.filter(v =>
      v.major == major && v.minor == minor && v.patch > patch
    ).maxOption
}
