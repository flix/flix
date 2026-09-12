/*
 * Copyright 2026 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.PackageId
import ca.uwaterloo.flix.util.Result.{Err, Ok, traverse}
import ca.uwaterloo.flix.util.{Result, Sha256}
import org.tomlj.{Toml, TomlInvalidTypeException, TomlParseResult, TomlTable}

import java.io.{IOException, StringReader}
import java.nio.file.Path
import scala.jdk.CollectionConverters.SetHasAsScala

object EffectLockfileParser {

  /**
    * The tables a lock file may contain.
    */
  private val AllowedTables: Set[String] = Set("lock", "packages")

  /**
    * The keys the `lock` table may contain.
    */
  private val AllowedLockKeys: Set[String] = Set("lock.version")

  /**
    * The tables the entry of a package may contain.
    */
  private val AllowedPackageTables: Set[String] = Set("defs", "sigs")

  /**
    * Creates an [[EffectLockfile]] from the toml file at `p`, and returns an error if the file
    * cannot be read or is not a lock file this version of Flix understands.
    */
  def parse(p: Path): Result[EffectLockfile, EffectLockError] = {
    val parser = try {
      Toml.parse(p)
    } catch {
      case e: IOException => return Err(EffectLockError.IOError(p, e.getMessage))
    }
    mkLockfile(parser, p)
  }

  /**
    * Creates an [[EffectLockfile]] from the string `s`, which should have the toml format. The
    * path `p` is where `s` comes from, and is used to report errors.
    */
  def parse(s: String, p: Path): Result[EffectLockfile, EffectLockError] = {
    val stringReader = new StringReader(s)
    val parser = try {
      Toml.parse(stringReader)
    } catch {
      case e: IOException => return Err(EffectLockError.IOError(p, e.getMessage))
    }
    mkLockfile(parser, p)
  }

  /**
    * Creates an [[EffectLockfile]] from `parser`, which should hold the lock file at `p`.
    */
  private def mkLockfile(parser: TomlParseResult, p: Path): Result[EffectLockfile, EffectLockError] = {
    val errors = parser.errors
    if (errors.size() > 0) {
      val sb = new StringBuilder()
      errors.forEach { error =>
        if (sb.nonEmpty) {
          sb.append(", ")
        }
        sb.append(error.toString)
      }
      return Err(EffectLockError.LockParseError(p, sb.toString))
    }

    for (
      _ <- checkKeys(parser, p);
      _ <- checkVersion(parser, p);
      packages <- collectPackages(parser, p)
    ) yield EffectLockfile(packages)
  }

  /**
    * Returns an error if the lock file at `p` contains a table or a `lock` key that this version
    * of Flix does not write.
    *
    * A lock file is written by Flix and read back by Flix, so anything else in it was put there
    * by a hand or a merge that did not mean to, and is worth reporting.
    */
  private def checkKeys(parser: TomlParseResult, p: Path): Result[Unit, EffectLockError] = {
    val tables = parser.keySet().asScala.toSet
    val illegalTables = tables.diff(AllowedTables)
    if (illegalTables.nonEmpty) {
      return Err(EffectLockError.IllegalTableFound(p, illegalTables.head))
    }

    val dottedKeys = parser.dottedKeySet().asScala.toSet
    val lockKeys = dottedKeys.filter(s => s.startsWith("lock."))
    val illegalLockKeys = lockKeys.diff(AllowedLockKeys)
    if (illegalLockKeys.nonEmpty) {
      return Err(EffectLockError.IllegalLockKeyFound(p, illegalLockKeys.head))
    }

    Ok(())
  }

  /**
    * Returns an error if the lock file at `p` is not written in the version of the lock file
    * format that this version of Flix understands.
    */
  private def checkVersion(parser: TomlParseResult, p: Path): Result[Unit, EffectLockError] = {
    val version = try {
      parser.getLong("lock.version")
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(EffectLockError.PropertyHasWrongType(p, "lock.version", "Integer", e.getMessage))
    }

    if (version == null) {
      return Err(EffectLockError.MissingRequiredProperty(p, "lock.version"))
    }

    if (version.longValue() != EffectLockfile.CurrentVersion) {
      return Err(EffectLockError.UnsupportedLockVersion(p, version.longValue()))
    }

    Ok(())
  }

  /**
    * Returns what every package in the lock file at `p` is locked at.
    *
    * A lock file with no `packages` table locks no package, which is what a project with no Flix
    * dependencies locks.
    */
  private def collectPackages(parser: TomlParseResult, p: Path): Result[Map[PackageId, LockedPackage], EffectLockError] = {
    val packages = try {
      parser.getTable("packages")
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(EffectLockError.PropertyHasWrongType(p, "packages", "Table", e.getMessage))
    }

    if (packages == null) {
      return Ok(Map.empty)
    }

    // A key that is not an identifier Flix could have written names no package, so it matches no
    // dependency. It is dropped here, and so is not written back the next time the file is written.
    val ids = packages.keySet().asScala.toList.flatMap(PackageId.mkPackageId).sorted
    traverse(ids)(id => collectPackage(packages, id, p).map(locked => id -> locked)).map(_.toMap)
  }

  /**
    * Returns what `packages` locks `id` at.
    *
    * A lock file that names a package the project does not depend on is not an error: nothing
    * matches the entry, and it is dropped the next time the file is written.
    */
  private def collectPackage(packages: TomlTable, id: PackageId, p: Path): Result[LockedPackage, EffectLockError] = {
    val identifier = id.toString
    // The identifier contains `:` and `/`, so it has to be quoted to be looked up as one key.
    val key = s"\"$identifier\""

    val entry = try {
      packages.getTable(key)
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(EffectLockError.PropertyHasWrongType(p, identifier, "Table", e.getMessage))
    }

    if (entry == null) {
      return Err(EffectLockError.PropertyHasWrongType(p, identifier, "Table", s"$identifier is not a table"))
    }

    val illegalTables = entry.keySet().asScala.toSet.diff(AllowedPackageTables)
    if (illegalTables.nonEmpty) {
      return Err(EffectLockError.IllegalPackageTableFound(p, identifier, illegalTables.head))
    }

    for (
      defs <- collectSignatures(entry, identifier, "defs", p);
      sigs <- collectSignatures(entry, identifier, "sigs", p)
    ) yield LockedPackage(defs, sigs)
  }

  /**
    * Returns the hash that the `table` of the package `identifier` holds for each symbol, where
    * `table` is `defs` or `sigs`.
    *
    * A table that is not there locks nothing, which is what a package with no public declaration
    * of that kind locks.
    */
  private def collectSignatures(entry: TomlTable, identifier: String, table: String, p: Path): Result[Map[String, Sha256], EffectLockError] = {
    val signatures = try {
      entry.getTable(table)
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(EffectLockError.PropertyHasWrongType(p, s"$identifier.$table", "Table", e.getMessage))
    }

    if (signatures == null) {
      return Ok(Map.empty)
    }

    val syms = signatures.keySet().asScala.toList.sorted
    traverse(syms)(sym => collectHash(signatures, identifier, table, sym, p).map(hash => sym -> hash)).map(_.toMap)
  }

  /**
    * Returns the hash that `signatures`, the `table` of the package `identifier`, holds for `sym`.
    */
  private def collectHash(signatures: TomlTable, identifier: String, table: String, sym: String, p: Path): Result[Sha256, EffectLockError] = {
    val property = s"$identifier.$table.$sym"
    // A symbol contains `.`, so it has to be quoted to be looked up as one key.
    val key = s"\"$sym\""

    val hash = try {
      signatures.getString(key)
    } catch {
      case _: IllegalArgumentException => null
      case e: TomlInvalidTypeException =>
        return Err(EffectLockError.PropertyHasWrongType(p, property, "String", e.getMessage))
    }

    if (hash == null) {
      return Err(EffectLockError.MissingRequiredProperty(p, property))
    }

    Sha256.parse(hash) match {
      case None => Err(EffectLockError.IllegalHash(p, identifier, sym, hash))
      case Some(sha256) => Ok(sha256)
    }
  }

}
