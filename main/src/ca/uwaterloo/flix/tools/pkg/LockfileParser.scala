/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.util.{Result, Sha256}
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse, traverseOpt}
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
    * The keys the entry of a package at a version may contain.
    */
  private val AllowedEntryKeys: Set[String] = Set("toml", "fpkg")

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
    mkLockFile(parser, p)
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
    mkLockFile(parser, p)
  }

  /**
    * Creates a [[Lockfile]] from `parser`, which should hold the lock file at `p`.
    */
  private def mkLockFile(parser: TomlParseResult, p: Path): Result[Lockfile, LockError] = {
    val errors = parser.errors
    if (errors.size() > 0) {
      val sb = new StringBuilder()
      errors.forEach { error =>
        if (sb.nonEmpty) {
          sb.append(", ")
        }
        sb.append(error.toString)
      }
      return Err(LockError.LockParseError(p, sb.toString))
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
    * Returns the entry of every package at every version in the lock file at `p`.
    *
    * A lock file with no `packages` table records no packages, which is what a project with no
    * Flix dependencies locks.
    */
  private def collectPackages(parser: TomlParseResult, p: Path): Result[Map[(PackageId, SemVer), LockEntry], LockError] = {
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

    // A key that is not an identifier Flix could have written names no package, so it matches no
    // dependency. It is dropped here, and so is not written back the next time the file is written.
    val ids = packages.keySet().asScala.toList.flatMap(PackageId.mkPackageId).sorted
    traverse(ids)(id => collectVersions(packages, id, p)).map(_.flatten.toMap)
  }

  /**
    * Returns the entry that `packages` holds for every version of `id`.
    *
    * A lock file that names a package the project does not depend on is not an error: the entry
    * is simply not one that any dependency matches, and it is dropped the next time the file is
    * written.
    */
  private def collectVersions(packages: TomlTable, id: PackageId, p: Path): Result[List[((PackageId, SemVer), LockEntry)], LockError] = {
    val identifier = id.toString
    // The identifier contains `:` and `/`, so it has to be quoted to be looked up as one key.
    val key = s"\"$identifier\""

    val versions = try {
      packages.getTable(key)
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(LockError.PropertyHasWrongType(p, identifier, "Table", e.getMessage))
    }

    if (versions == null) {
      return Err(LockError.PropertyHasWrongType(p, identifier, "Table", s"$identifier is not a table"))
    }

    traverse(versions.keySet().asScala.toList.sorted) { versionKey =>
      for (
        version <- toSemVer(versionKey, identifier, p);
        entry <- collectEntry(versions, identifier, versionKey, p)
      ) yield ((id, version), entry)
    }
  }

  /**
    * Returns the entry that `versions`, the table of the package `identifier`, holds for the
    * version written as `versionKey`.
    */
  private def collectEntry(versions: TomlTable, identifier: String, versionKey: String, p: Path): Result[LockEntry, LockError] = {
    val qualified = s"packages.\"$identifier\".\"$versionKey\""
    // The version contains `.`, so it has to be quoted to be looked up as one key.
    val key = s"\"$versionKey\""

    val entry = try {
      versions.getTable(key)
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(LockError.PropertyHasWrongType(p, qualified, "Table", e.getMessage))
    }

    if (entry == null) {
      return Err(LockError.PropertyHasWrongType(p, qualified, "Table", s"$qualified is not a table"))
    }

    val illegalKeys = entry.keySet().asScala.toSet.diff(AllowedEntryKeys)
    if (illegalKeys.nonEmpty) {
      return Err(LockError.IllegalPackageKeyFound(p, identifier, illegalKeys.head))
    }

    for (
      toml <- getString(entry, qualified, "toml", p).flatMap {
        case Some(digest) => Ok(digest)
        case None => Err(LockError.MissingRequiredProperty(p, s"$qualified.toml"))
      };
      tomlDigest <- toDigest(toml, identifier, "toml", p);

      // An entry records an fpkg only if the package has been downloaded at the version.
      fpkg <- getString(entry, qualified, "fpkg", p);
      fpkgDigest <- traverseOpt(fpkg)(toDigest(_, identifier, "fpkg", p))
    ) yield LockEntry(tomlDigest, fpkgDigest)
  }

  /**
    * Returns the string that `entry` holds at `property`, if it holds one, and an error if it
    * holds something that is not a string. The `qualified` name is that of `entry`, and is used
    * to report errors.
    */
  private def getString(entry: TomlTable, qualified: String, property: String, p: Path): Result[Option[String], LockError] = {
    try {
      Ok(Option(entry.getString(property)))
    } catch {
      case _: IllegalArgumentException => Ok(None)
      case e: TomlInvalidTypeException => Err(LockError.PropertyHasWrongType(p, s"$qualified.$property", "String", e.getMessage))
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
