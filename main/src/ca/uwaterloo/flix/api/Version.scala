/*
 * Copyright 2015-2016 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.api

object Version {
  /**
    * Represents the current version of Flix.
    */
  val CurrentVersion: Version = Version(major = 0, minor = 75, revision = 3)
}

/**
  * A case class to represent versions.
  */
case class Version(major: Int, minor: Int, revision: Int) {
  override val toString: String = s"$major.$minor.$revision"
}
