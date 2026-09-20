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

import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Path

/**
  * A common super-type for errors in an `effects.lock` file.
  */
sealed trait EffectLockError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object EffectLockError {

  /**
    * An error raised when a hash recorded in the lock file is not one Flix could have written.
    *
    * A hash is written as 'sha256:' followed by 64 lowercase hexadecimal characters. Anything
    * else was put there by a hand or a merge, since Flix writes no other form.
    *
    * @param path       the path of the lock file.
    * @param identifier the identifier of the package the hash belongs to, e.g. `github:flix/museum`.
    * @param symbol     the symbol the hash belongs to, e.g. `Museum.Clerk.sell`.
    * @param hash       the hash that was found.
    */
  case class IllegalHash(path: Path, identifier: String, symbol: String, hash: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The hash of ${f.bold(symbol)} of ${f.bold(identifier)} should be formatted like so: 'sha256:' followed by 64 lowercase hexadecimal characters.
         |Instead found: ${f.red(hash)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the `lock` table holds a key that Flix does not write.
    *
    * @param path the path of the lock file.
    * @param key  the key that was found.
    */
  case class IllegalLockKeyFound(path: Path, key: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The ${f.bold("lock")} table should hold no key other than ${f.bold("version")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the entry of a package holds a table that Flix does not write.
    *
    * @param path       the path of the lock file.
    * @param identifier the identifier of the package, e.g. `github:flix/museum`.
    * @param key        the key that was found.
    */
  case class IllegalPackageTableFound(path: Path, identifier: String, key: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The entry of ${f.bold(identifier)} should hold no table other than ${f.bold("defs")} and ${f.bold("sigs")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the lock file holds a table that Flix does not write.
    *
    * @param path the path of the lock file.
    * @param key  the key that was found.
    */
  case class IllegalTableFound(path: Path, key: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The lock file should hold no table other than ${f.bold("lock")} and ${f.bold("packages")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the lock file cannot be read.
    *
    * @param path    the path of the lock file.
    * @param message what went wrong.
    */
  case class IOError(path: Path, message: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The lock file at ${f.cyan(path.toString)} could not be read.
         |${f.red(message)}
         |""".stripMargin
  }

  /**
    * An error raised when the lock file is not a toml file.
    *
    * @param path    the path of the lock file.
    * @param message what the toml parser said.
    */
  case class LockParseError(path: Path, message: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The lock file at ${f.cyan(path.toString)} is not a valid toml file.
         |${f.red(message)}
         |""".stripMargin
  }

  /**
    * An error raised when the lock file does not hold a property that Flix writes.
    *
    * @param path     the path of the lock file.
    * @param property the property that is missing.
    */
  case class MissingRequiredProperty(path: Path, property: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The lock file should hold the property ${f.bold(property)}, but does not.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when a property of the lock file is not of the type Flix writes it as.
    *
    * @param path         the path of the lock file.
    * @param property     the property.
    * @param requiredType the type the property should have.
    * @param message      what the toml parser said.
    */
  case class PropertyHasWrongType(path: Path, property: String, requiredType: String, message: String) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The property ${f.bold(property)} should be of type ${f.bold(requiredType)}.
         |${f.red(message)}
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the lock file is written in a version of the format that this version
    * of Flix does not understand.
    *
    * @param path    the path of the lock file.
    * @param version the version the lock file is written in.
    */
  case class UnsupportedLockVersion(path: Path, version: Long) extends EffectLockError {
    override def message(f: Formatter): String =
      s"""The lock file is written in version ${f.red(version.toString)} of the format, but this version of Flix writes version ${f.bold(EffectLockfile.CurrentVersion.toString)}.
         |Run ${f.bold("flix eff-lock")} to write it anew, after checking that the signatures it records are the ones you expect.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

}
