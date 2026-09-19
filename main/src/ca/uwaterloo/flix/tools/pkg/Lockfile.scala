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

object Lockfile {

  /**
    * The version of the lock file format that this version of Flix writes.
    *
    * Recorded in every lock file so that a version of Flix which records something different can
    * say so, instead of reading the file as though it were its own.
    */
  val CurrentVersion: Long = 1L

  /**
    * The line separator used in a lock file.
    *
    * A lock file is committed and rewritten on every build, so its bytes must not depend on the
    * platform that wrote it. `System.lineSeparator` would make the file churn between a developer
    * on Windows and one on macOS or Linux.
    */
  private val NewLine: String = "\n"

  /**
    * Formats `lockfile` as the contents of a `packages.lock` file.
    *
    * Parsing the output yields the original lock file, i.e. `lockfile`.
    *
    * Entries are written in order of identifier, and then of version, so that the same
    * resolution always produces the same file, whichever order the packages happened to be
    * installed in.
    */
  def format(lockfile: Lockfile): String = {
    val sb = new StringBuilder()

    sb.append("[lock]").append(NewLine)
    sb.append("version = ").append(CurrentVersion).append(NewLine)

    val entries = lockfile.packages.toList.sortBy { case (node, _) => node }
    for (((id, version), entry) <- entries) {
      sb.append(NewLine)
      appendEntry(sb, id, version, entry)
    }

    sb.toString
  }

  /**
    * Appends the entry of `id` at `version` to `sb`.
    *
    * The identifier and the version are written as quoted keys, so that the `:`, `/` and `.` they
    * contain are part of the key rather than separators. They need no escaping: every part of a
    * [[PackageId]] is alphanumeric, and a version is digits and dots.
    */
  private def appendEntry(sb: StringBuilder, id: PackageId, version: SemVer, entry: LockEntry): Unit = {
    sb.append("[packages.\"").append(id).append("\".\"").append(version).append("\"]").append(NewLine)
    sb.append("toml    = \"").append(entry.toml).append("\"").append(NewLine)
    for (fpkg <- entry.fpkg) {
      sb.append("fpkg    = \"").append(fpkg).append("\"").append(NewLine)
    }
  }

}

/**
  * The digests of every Flix package that a project depends on, as they were when the package was
  * last downloaded.
  *
  * A project has one lock file, `packages.lock`, next to its `flix.toml`, and it is committed. It
  * records what each dependency was, so that a later build can tell that a dependency is no
  * longer the same and refuse to compile it.
  *
  * The version of the lock file format is not carried here. A lock file is always written in
  * [[Lockfile.CurrentVersion]], and [[LockfileParser]] refuses to read a file written in any
  * other, so there is no version for a caller to choose.
  *
  * A lock file is a set of facts, each of which says what one file of one package at one version
  * is. It records the `flix.toml` of every package at every version that the dependency graph
  * requires, because each of them is read to resolve the graph, and the `.fpkg` of those that
  * have been downloaded. It does not say which version a package is built at: that is for the
  * resolution to decide, see [[FlixPackageManager.resolve]], and the lock file only says what the
  * files that the resolution reads must be. Several versions of a package may each record an
  * `.fpkg`, and none of them contradicts another.
  *
  * A project that requires `museum`, where `museum-clerk` is required at two versions, locks them
  * like so:
  *
  * {{{
  * [lock]
  * version = 1
  *
  * [packages."github:flix/museum"."1.2.3"]
  * toml    = "sha256:a4c123b1612dd272d1371c17149d439536b3216fdaeeb975729fae923d5a4fd1"
  * fpkg    = "sha256:2aabfe228f219e9cb0eb53f16947ccf25ec84d8dbc74254770f58904dba41ecc"
  *
  * [packages."github:flix/museum-clerk"."0.4.0"]
  * toml    = "sha256:2ab1a5b8f6cbd0ba5b0e0b9f3b8d8b3d2b9c0e0a4a7f6f5e4d3c2b1a09f8e7d6"
  *
  * [packages."github:flix/museum-clerk"."0.4.1"]
  * toml    = "sha256:cc3fc1626e53a13043b026c48bbf33feff9243a8f506b40928b5b7a767c76fb0"
  * fpkg    = "sha256:08f86bebb2737f6a6f0fb23c6f5da2cec255404e4fb440034d6608697a8d41be"
  * }}}
  *
  * @param packages the entry of each package at each version, e.g. `github:flix/museum` at `1.2.3`.
  */
case class Lockfile(packages: Map[(PackageId, SemVer), LockEntry])
