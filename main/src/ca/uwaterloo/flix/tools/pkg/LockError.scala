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

import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Path

sealed trait LockError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object LockError {

  case class IOError(path: Path, message: String) extends LockError {
    override def message(f: Formatter): String =
      s"""An I/O error occurred while reading the lock file at ${f.cyan(path.toString)}.
         |Error: ${f.red(message)}
         |""".stripMargin
  }

  case class LockParseError(path: Path, message: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The lock file at ${f.cyan(path.toString)} is not valid toml.
         |Error: ${f.red(message)}
         |""".stripMargin
  }

  case class MissingRequiredProperty(path: Path, property: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The lock file does not contain a required property called ${f.bold(property)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  case class PropertyHasWrongType(path: Path, property: String, requiredType: String, message: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The property ${f.bold(property)} is required to have a value of type ${f.bold(requiredType)}.
         |Error: ${f.red(message)}
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  case class IllegalTableFound(path: Path, key: String) extends LockError {
    override def message(f: Formatter): String =
      s"""A lock file should only contain the tables ${f.bold("lock")} and ${f.bold("packages")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  case class IllegalLockKeyFound(path: Path, key: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The ${f.bold("lock")} table should only contain the key ${f.bold("version")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  case class IllegalPackageKeyFound(path: Path, identifier: String, key: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The entry for ${f.bold(identifier)} should only contain the keys ${f.bold("version")}, ${f.bold("toml")}, and ${f.bold("fpkg")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * The lock file was written by a version of Flix that records something this one does not
    * understand. The file cannot be repaired by guessing at its contents, so it is reported
    * rather than ignored.
    */
  case class UnsupportedLockVersion(path: Path, version: Long) extends LockError {
    override def message(f: Formatter): String =
      s"""The lock file is written in version ${f.bold(version.toString)} of the lock file format.
         |This version of Flix understands version ${f.bold(Lockfile.CurrentVersion.toString)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  case class IllegalVersion(path: Path, identifier: String, version: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The version of ${f.bold(identifier)} should be formatted like so: 'x.x.x'.
         |Instead found: ${f.red(version)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  case class IllegalDigest(path: Path, identifier: String, property: String, digest: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The ${f.bold(property)} digest of ${f.bold(identifier)} should be formatted like so: 'sha256:' followed by 64 lowercase hexadecimal characters.
         |Instead found: ${f.red(digest)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

}
