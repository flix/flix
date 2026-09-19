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

import ca.uwaterloo.flix.language.ast.shared.PackageId

object PackageSpec {

  /** The host of a specification that names none. */
  private val DefaultHost: String = "github"

  /** The separator between a package and the version that is asked of it. */
  private val VersionSeparator: Char = '@'

  /** The separator between the host and the rest of a package identifier. */
  private val HostSeparator: Char = ':'

  /**
    * Returns `s` as a package specification, if it is one.
    *
    * A specification is a package identifier, optionally followed by `@` and a version:
    * `flix/museum-clerk`, `github:flix/museum-clerk`, and `flix/museum-clerk@1.2.3` are all
    * specifications. The host may be left out, since `github` is the only one there is.
    *
    * No part of an identifier may contain `@` or `:`, so neither split can be ambiguous.
    */
  def mkPackageSpec(s: String): Option[PackageSpec] = {
    val (identifier, versionString) = s.indexOf(VersionSeparator) match {
      case -1 => (s, None)
      case i => (s.substring(0, i), Some(s.substring(i + 1)))
    }

    val qualified = if (identifier.indexOf(HostSeparator) >= 0) identifier else s"$DefaultHost$HostSeparator$identifier"

    for {
      id <- PackageId.mkPackageId(qualified)
      // A version that is written must be one, rather than fall back to the latest release.
      version <- versionString match {
        case None => Some(None)
        case Some(v) => SemVer.ofString(v).map(Some.apply)
      }
    } yield PackageSpec(id, version)
  }

}

/**
  * A package, and the version of it that is asked for, if one is.
  *
  * A specification that asks for no version means the newest release of the package.
  */
case class PackageSpec(id: PackageId, version: Option[SemVer]) {
  override def toString: String = version.map(v => s"$id@$v").getOrElse(id.toString)
}
