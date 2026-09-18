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
    * Formats `lockfile` as the contents of a `flix.lock` file.
    *
    * Parsing the output yields the original lock file, i.e. `lockfile`.
    *
    * Entries are written in order of identifier, so that the same resolution always produces the
    * same file, whichever order the packages happened to be installed in.
    */
  def format(lockfile: Lockfile): String = {
    val sb = new StringBuilder()

    sb.append("[lock]").append(NewLine)
    sb.append("version = ").append(CurrentVersion).append(NewLine)

    val entries = lockfile.packages.toList.sortBy { case (identifier, _) => identifier }
    for ((identifier, entry) <- entries) {
      sb.append(NewLine)
      appendEntry(sb, identifier, entry)
    }

    sb.toString
  }

  /**
    * Appends the entry of the package named by `identifier` to `sb`.
    *
    * The identifier is written as a quoted key, so that the `:` and `/` it contains are part of
    * the key rather than separators. It needs no escaping: an identifier is built from a
    * repository name and the two names of a GitHub project, which [[ManifestParser]] has already
    * checked to be alphanumeric.
    */
  private def appendEntry(sb: StringBuilder, identifier: String, entry: LockEntry): Unit = {
    sb.append("[packages.\"").append(identifier).append("\"]").append(NewLine)
    sb.append("version = \"").append(entry.version).append("\"").append(NewLine)
    sb.append("toml    = \"").append(entry.toml).append("\"").append(NewLine)
    sb.append("fpkg    = \"").append(entry.fpkg).append("\"").append(NewLine)
  }

}

/**
  * The digests of every Flix package that a project depends on, as they were when the package was
  * last downloaded.
  *
  * A project has one lock file, `flix.lock`, next to its `flix.toml`, and it is committed. It
  * records what each dependency was, so that a later build can tell that a dependency is no
  * longer the same and refuse to compile it.
  *
  * The version of the lock file format is not carried here. A lock file is always written in
  * [[Lockfile.CurrentVersion]], and [[LockfileParser]] refuses to read a file written in any
  * other, so there is no version for a caller to choose.
  *
  * A project that depends on two packages locks them like so:
  *
  * {{{
  * [lock]
  * version = 1
  *
  * [packages."github:flix/museum"]
  * version = "1.2.3"
  * toml    = "sha256:a4c123b1612dd272d1371c17149d439536b3216fdaeeb975729fae923d5a4fd1"
  * fpkg    = "sha256:2aabfe228f219e9cb0eb53f16947ccf25ec84d8dbc74254770f58904dba41ecc"
  *
  * [packages."github:flix/museum-clerk"]
  * version = "0.4.0"
  * toml    = "sha256:cc3fc1626e53a13043b026c48bbf33feff9243a8f506b40928b5b7a767c76fb0"
  * fpkg    = "sha256:08f86bebb2737f6a6f0fb23c6f5da2cec255404e4fb440034d6608697a8d41be"
  * }}}
  *
  * @param packages the entry of each package, by the identifier of that package, e.g.
  *                 `github:flix/museum`.
  */
case class Lockfile(packages: Map[String, LockEntry])
