/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.util.Formatter

import java.nio.file.Path

/**
  * A common super-type for errors in a `packages.lock` file.
  */
sealed trait LockError {
  /**
    * Returns a human-readable and formatted string representation of this error.
    */
  def message(f: Formatter): String
}

object LockError {

  /**
    * An error raised when a digest recorded in the lock file is not one Flix could have written.
    *
    * A digest is written as 'sha256:' followed by 64 lowercase hexadecimal characters. Anything
    * else was put there by a hand or a merge, since Flix writes no other form.
    *
    * @param path       the path of the lock file.
    * @param identifier the identifier of the package the digest belongs to, e.g. `github:flix/museum`.
    * @param property   the file the digest belongs to, i.e. `toml` or `fpkg`.
    * @param digest     the digest that was found.
    */
  case class IllegalDigest(path: Path, identifier: String, property: String, digest: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The ${f.bold(property)} digest of ${f.bold(identifier)} should be formatted like so: 'sha256:' followed by 64 lowercase hexadecimal characters.
         |Instead found: ${f.red(digest)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the `lock` table holds a key that Flix does not write.
    *
    * @param path the path of the lock file.
    * @param key  the key that was found.
    */
  case class IllegalLockKeyFound(path: Path, key: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The ${f.bold("lock")} table should only contain the key ${f.bold("version")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the entry of a package holds a key that Flix does not write.
    *
    * @param path       the path of the lock file.
    * @param identifier the identifier of the package, e.g. `github:flix/museum`.
    * @param key        the key that was found.
    */
  case class IllegalPackageKeyFound(path: Path, identifier: String, key: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The entry for ${f.bold(identifier)} should only contain the keys ${f.bold("version")}, ${f.bold("toml")}, and ${f.bold("fpkg")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the lock file holds a table that Flix does not write.
    *
    * @param path the path of the lock file.
    * @param key  the name of the table that was found.
    */
  case class IllegalTableFound(path: Path, key: String) extends LockError {
    override def message(f: Formatter): String =
      s"""A lock file should only contain the tables ${f.bold("lock")} and ${f.bold("packages")}.
         |Instead found: ${f.red(key)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the version recorded for a package is not a semantic version.
    *
    * @param path       the path of the lock file.
    * @param identifier the identifier of the package, e.g. `github:flix/museum`.
    * @param version    the version that was found.
    */
  case class IllegalVersion(path: Path, identifier: String, version: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The version of ${f.bold(identifier)} should be formatted like so: 'x.x.x'.
         |Instead found: ${f.red(version)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the lock file cannot be read.
    *
    * @param path    the path of the lock file.
    * @param message the message of the underlying I/O error.
    */
  case class IOError(path: Path, message: String) extends LockError {
    override def message(f: Formatter): String =
      s"""An I/O error occurred while reading the lock file at ${f.cyan(path.toString)}.
         |Error: ${f.red(message)}
         |""".stripMargin
  }

  /**
    * An error raised when the lock file is not valid toml.
    *
    * @param path    the path of the lock file.
    * @param message the messages of the underlying toml errors.
    */
  case class LockParseError(path: Path, message: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The lock file at ${f.cyan(path.toString)} is not valid toml.
         |Error: ${f.red(message)}
         |""".stripMargin
  }

  /**
    * An error raised when the lock file does not record something that every lock file records.
    *
    * For example, a lock file that does not say which version of the lock file format it is
    * written in, or an entry that records a package with no digest for its `.fpkg`.
    *
    * @param path     the path of the lock file.
    * @param property the property that is missing, e.g. `lock.version`.
    */
  case class MissingRequiredProperty(path: Path, property: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The lock file does not contain a required property called ${f.bold(property)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when a property of the lock file holds a value of the wrong type.
    *
    * For example, a version recorded as an integer rather than as a string.
    *
    * @param path         the path of the lock file.
    * @param property     the property that holds the value, e.g. `lock.version`.
    * @param requiredType the name of the type the property is required to hold.
    * @param message      the message of the underlying toml error.
    */
  case class PropertyHasWrongType(path: Path, property: String, requiredType: String, message: String) extends LockError {
    override def message(f: Formatter): String =
      s"""The property ${f.bold(property)} is required to have a value of type ${f.bold(requiredType)}.
         |Error: ${f.red(message)}
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

  /**
    * An error raised when the lock file is written in a version of the lock file format that this
    * version of Flix does not understand.
    *
    * The file records something this version of Flix does not know how to read, and it cannot be
    * repaired by guessing at its contents, so it is reported rather than ignored.
    *
    * @param path    the path of the lock file.
    * @param version the version of the lock file format that was found.
    */
  case class UnsupportedLockVersion(path: Path, version: Long) extends LockError {
    override def message(f: Formatter): String =
      s"""The lock file is written in version ${f.bold(version.toString)} of the lock file format.
         |This version of Flix understands version ${f.bold(Lockfile.CurrentVersion.toString)}.
         |The lock file was found at ${f.cyan(path.toString)}.
         |""".stripMargin
  }

}
