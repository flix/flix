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
import ca.uwaterloo.flix.util.Sha256

object EffectLockfile {

  /**
    * The version of the lock file format that this version of Flix writes.
    *
    * Recorded in every lock file so that a version of Flix which records something different can
    * say so, instead of reading the file as though it were its own.
    *
    * The version covers what a hash in the file means as well as how the file is laid out, since
    * a reader can do nothing with a hash it would compute differently. A change to
    * [[HashType.Version]] must therefore bump this one too.
    */
  val CurrentVersion: Long = 1L

  /**
    * The line separator used in a lock file.
    *
    * A lock file is committed and rewritten whenever a package is locked, so its bytes must not
    * depend on the platform that wrote it. `System.lineSeparator` would make the file churn
    * between a developer on Windows and one on macOS or Linux.
    */
  private val NewLine: String = "\n"

  /**
    * Formats `lockfile` as the contents of an `effects.lock` file.
    *
    * Parsing the output yields the original lock file, i.e. `lockfile`.
    *
    * Packages are written in order of identifier, and the signatures of a package in order of
    * symbol, so that the same program always produces the same file, whichever order the
    * declarations happened to be compiled in.
    */
  def format(lockfile: EffectLockfile): String = {
    val sb = new StringBuilder()

    sb.append("[lock]").append(NewLine)
    sb.append("version = ").append(CurrentVersion).append(NewLine)

    for ((id, locked) <- lockfile.packages.toList.sortBy { case (id, _) => id }) {
      appendSignatures(sb, id, "defs", locked.defs)
      appendSignatures(sb, id, "sigs", locked.sigs)
    }

    sb.toString
  }

  /**
    * Appends the `table` of the package `id` to `sb`, where `table` is `defs` or `sigs`.
    *
    * A table that locks nothing is left out: a package that has no public declaration of that
    * kind has nothing to record, and an empty table would say the same thing at more length.
    *
    * The identifier of the package and the name of each symbol are written as quoted keys, so
    * that the `:`, `/` and `.` they contain are part of the key rather than separators. They need
    * no escaping: every part of a [[PackageId]] is alphanumeric, and a symbol is named by
    * identifiers joined by dots.
    */
  private def appendSignatures(sb: StringBuilder, id: PackageId, table: String, signatures: Map[String, Sha256]): Unit = {
    if (signatures.isEmpty) {
      return
    }

    sb.append(NewLine)
    sb.append("[packages.\"").append(id).append("\".").append(table).append("]").append(NewLine)
    for ((sym, hash) <- signatures.toList.sortBy { case (sym, _) => sym }) {
      sb.append("\"").append(sym).append("\" = \"").append(hash).append("\"").append(NewLine)
    }
  }

}

/**
  * The hash of the signature of every public declaration of every package that a project locks.
  *
  * A project has one effect lock file, `effects.lock`, next to its `flix.toml`, and it is
  * committed. It records what the public signatures of a dependency were when the project last
  * locked it, so that a later build can tell that a dependency no longer has the signatures it
  * was taken on and refuse to compile it.
  *
  * A signature is recorded by its hash and nothing else, so the file says whether a signature is
  * the one that was locked. It does not say what that signature was, and it cannot be read back
  * into a type: a change is a change, whether it widens what the dependency may do or narrows it.
  *
  * The version of the lock file format is not carried here. A lock file is always written in
  * [[EffectLockfile.CurrentVersion]], and [[EffectLockfileParser]] refuses to read a file written
  * in any other, so there is no version for a caller to choose.
  *
  * A package is a section of its own, so the lock of one package can be replaced without
  * disturbing the lock of another.
  *
  * A project that depends on `museum-clerk` locks it like so:
  *
  * {{{
  * [lock]
  * version = 1
  *
  * [packages."github:flix/museum-clerk".defs]
  * "Museum.Clerk.buy"  = "sha256:7dad1579ec59d87919c1fecc880115ed78f96e44f297b46a841c6989facc6128"
  * "Museum.Clerk.sell" = "sha256:ed1223de424e1f84a12845f463ff9592a8c1a8f17f82c4afd841e80f5958f82e"
  *
  * [packages."github:flix/museum-clerk".sigs]
  * "Museum.Clerk.Sellable.price" = "sha256:24067c6b1ee9d26b702a8857bc3c20384f3592b8e5013b5052f292823cdc3020"
  * }}}
  *
  * @param packages what each package is locked at, e.g. `github:flix/museum-clerk`.
  */
case class EffectLockfile(packages: Map[PackageId, LockedPackage])
