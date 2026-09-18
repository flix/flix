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

import ca.uwaterloo.flix.util.{Result, Sha256}
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse}
import org.tomlj.{Toml, TomlInvalidTypeException, TomlParseResult, TomlTable}

import java.io.{IOException, StringReader}
import java.nio.file.Path
import scala.jdk.CollectionConverters.SetHasAsScala

object LockfileParser {

  /**
    * The tables a lock file may contain.
    */
  private val AllowedTables: Set[String] = Set("lock", "packages")

  /**
    * The keys the `lock` table may contain.
    */
  private val AllowedLockKeys: Set[String] = Set("lock.version")

  /**
    * The keys the entry of a package may contain.
    */
  private val AllowedPackageKeys: Set[String] = Set("version", "toml", "fpkg")

  /**
    * Creates a [[Lockfile]] from the toml file at `p`, and returns an error if the file cannot be
    * read or is not a lock file this version of Flix understands.
    */
  def parse(p: Path): Result[Lockfile, LockError] = {
    val parser = try {
      Toml.parse(p)
    } catch {
      case e: IOException => return Err(LockError.IOError(p, e.getMessage))
    }
    createLockfile(parser, p)
  }

  /**
    * Creates a [[Lockfile]] from the string `s`, which should have the toml format. The path `p`
    * is where `s` comes from, and is used to report errors.
    */
  def parse(s: String, p: Path): Result[Lockfile, LockError] = {
    val stringReader = new StringReader(s)
    val parser = try {
      Toml.parse(stringReader)
    } catch {
      case e: IOException => return Err(LockError.IOError(p, e.getMessage))
    }
    createLockfile(parser, p)
  }

  /**
    * Creates a [[Lockfile]] from `parser`, which should hold the lock file at `p`.
    */
  private def createLockfile(parser: TomlParseResult, p: Path): Result[Lockfile, LockError] = {
    val errors = parser.errors
    if (errors.size() > 0) {
      var errorString = ""
      errors.forEach(error => errorString = errorString + error.toString + ", ")
      return Err(LockError.LockParseError(p, errorString))
    }

    for (
      _ <- checkKeys(parser, p);
      _ <- checkVersion(parser, p);
      packages <- collectPackages(parser, p)
    ) yield Lockfile(packages)
  }

  /**
    * Returns an error if the lock file at `p` contains a table or a `lock` key that this version
    * of Flix does not write.
    *
    * A lock file is written by Flix and read back by Flix, so anything else in it was put there
    * by a hand or a merge that did not mean to, and is worth reporting.
    */
  private def checkKeys(parser: TomlParseResult, p: Path): Result[Unit, LockError] = {
    val tables = parser.keySet().asScala.toSet
    val illegalTables = tables.diff(AllowedTables)
    if (illegalTables.nonEmpty) {
      return Err(LockError.IllegalTableFound(p, illegalTables.head))
    }

    val dottedKeys = parser.dottedKeySet().asScala.toSet
    val lockKeys = dottedKeys.filter(s => s.startsWith("lock."))
    val illegalLockKeys = lockKeys.diff(AllowedLockKeys)
    if (illegalLockKeys.nonEmpty) {
      return Err(LockError.IllegalLockKeyFound(p, illegalLockKeys.head))
    }

    Ok(())
  }

  /**
    * Returns an error if the lock file at `p` is not written in the version of the lock file
    * format that this version of Flix understands.
    */
  private def checkVersion(parser: TomlParseResult, p: Path): Result[Unit, LockError] = {
    val version = try {
      parser.getLong("lock.version")
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(LockError.PropertyHasWrongType(p, "lock.version", "Integer", e.getMessage))
    }

    if (version == null) {
      return Err(LockError.MissingRequiredProperty(p, "lock.version"))
    }

    if (version.longValue() != Lockfile.CurrentVersion) {
      return Err(LockError.UnsupportedLockVersion(p, version.longValue()))
    }

    Ok(())
  }

  /**
    * Returns the entry of every package in the lock file at `p`, by identifier.
    *
    * A lock file with no `packages` table records no packages, which is what a project with no
    * Flix dependencies locks.
    */
  private def collectPackages(parser: TomlParseResult, p: Path): Result[Map[String, LockEntry], LockError] = {
    val packages = try {
      parser.getTable("packages")
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(LockError.PropertyHasWrongType(p, "packages", "Table", e.getMessage))
    }

    if (packages == null) {
      return Ok(Map.empty)
    }

    val identifiers = packages.keySet().asScala.toSet
    traverse(identifiers)(identifier => collectPackage(packages, identifier, p)).map(_.toMap)
  }

  /**
    * Returns the entry that `packages` holds for the package named by `identifier`.
    *
    * The identifier is not checked to be one that Flix could have written. A lock file that names
    * a package the project does not depend on is not an error: the entry is simply not one that
    * any dependency matches, and it is dropped the next time the file is written.
    */
  private def collectPackage(packages: TomlTable, identifier: String, p: Path): Result[(String, LockEntry), LockError] = {
    // The identifier contains `:` and `/`, so it has to be quoted to be looked up as one key.
    val key = s"\"$identifier\""

    val entry = try {
      packages.getTable(key)
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(LockError.PropertyHasWrongType(p, identifier, "Table", e.getMessage))
    }

    if (entry == null) {
      return Err(LockError.PropertyHasWrongType(p, identifier, "Table", s"$identifier is not a table"))
    }

    val illegalKeys = entry.keySet().asScala.toSet.diff(AllowedPackageKeys)
    if (illegalKeys.nonEmpty) {
      return Err(LockError.IllegalPackageKeyFound(p, identifier, illegalKeys.head))
    }

    for (
      version <- getRequiredString(entry, identifier, "version", p);
      semVer <- toSemVer(version, identifier, p);

      toml <- getRequiredString(entry, identifier, "toml", p);
      tomlDigest <- toDigest(toml, identifier, "toml", p);

      fpkg <- getRequiredString(entry, identifier, "fpkg", p);
      fpkgDigest <- toDigest(fpkg, identifier, "fpkg", p)
    ) yield (identifier, LockEntry(semVer, tomlDigest, fpkgDigest))
  }

  /**
    * Returns the string that `entry` holds at `property`, and an error if it holds nothing there
    * or holds something that is not a string. The `identifier` names the package `entry` belongs
    * to, and is used to report errors.
    */
  private def getRequiredString(entry: TomlTable, identifier: String, property: String, p: Path): Result[String, LockError] = {
    val qualified = s"packages.\"$identifier\".$property"
    try {
      val result = entry.getString(property)
      if (result == null) {
        return Err(LockError.MissingRequiredProperty(p, qualified))
      }
      Ok(result)
    } catch {
      case _: IllegalArgumentException => Err(LockError.MissingRequiredProperty(p, qualified))
      case e: TomlInvalidTypeException => Err(LockError.PropertyHasWrongType(p, qualified, "String", e.getMessage))
    }
  }

  /**
    * Returns the semantic version that `s` denotes, and an error if it denotes none.
    */
  private def toSemVer(s: String, identifier: String, p: Path): Result[SemVer, LockError] = SemVer.ofString(s) match {
    case Some(semVer) => Ok(semVer)
    case None => Err(LockError.IllegalVersion(p, identifier, s))
  }

  /**
    * Returns the digest that `s` denotes, and an error if it denotes none. The `property` names
    * the file the digest belongs to, and is used to report errors.
    */
  private def toDigest(s: String, identifier: String, property: String, p: Path): Result[Sha256, LockError] = Sha256.parse(s) match {
    case Some(digest) => Ok(digest)
    case None => Err(LockError.IllegalDigest(p, identifier, property, s))
  }

}
