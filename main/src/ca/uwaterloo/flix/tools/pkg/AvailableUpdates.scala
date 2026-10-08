/*
 * Copyright 2024 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

case class AvailableUpdates(major: Option[SemVer], minor: Option[SemVer], patch: Option[SemVer]) {

  /**
    * Returns `true` if no updates are available.
    */
  def isEmpty: Boolean =
    major.isEmpty && minor.isEmpty && patch.isEmpty

}
