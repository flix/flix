/*
 * Copyright 2025 Jakob Schneider Villumsen
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

import ca.uwaterloo.flix.api.effectlock.serialization.{Deserialize, Serialize}
import ca.uwaterloo.flix.language.ast.{Scheme, Symbol, TypedAst}
import ca.uwaterloo.flix.language.ast.shared.{Origin, PackageId, Source}
import ca.uwaterloo.flix.util.Result

object EffectLock {

  /**
    * The effect lock of one package: the schemes its public defs and sigs were locked at.
    */
  case class LockedPackage(defs: Map[Symbol.DefnSym, Scheme], sigs: Map[Symbol.SigSym, Scheme])

  /**
    * The effect lock as it is written to disk: a map from each package to the serialized schemes
    * of its public defs and sigs, keyed by the symbol they belong to.
    *
    * A package is a section of its own, so the lock of one package can be replaced without
    * disturbing the lock of another.
    */
  type SerializedLock = Map[String, Map[String, serialization.DefOrSig]]

  /**
    * Returns the schemes of the public defs and sigs of `targets` in `root`.
    *
    * Only a package can be locked. The library bundled with the compiler cannot change under a
    * project, because the version of Flix that provides it is fixed by the manifest, so it has no
    * lock of its own.
    */
  def lock(root: TypedAst.Root, targets: Set[PackageId]): SerializedLock = {
    val defs = root.defs.toList.flatMap {
      case (sym, defn) if defn.spec.mod.isPublic =>
        packageOf(sym.src).filter(targets.contains).map(id => id -> (sym.toString -> Serialize.serializeDef(defn)))
      case _ => None
    }
    val sigs = root.sigs.toList.flatMap {
      case (sym, sig) if sig.spec.mod.isPublic =>
        packageOf(sym.src).filter(targets.contains).map(id => id -> (sym.toString -> Serialize.serializeSig(sig)))
      case _ => None
    }
    (defs ::: sigs).groupMap { case (id, _) => id.toString } { case (_, entry) => entry }.map {
      case (id, entries) => id -> entries.toMap
    }
  }

  /**
    * Returns `lock` as a JSON string that may be written directly to a file.
    */
  def format(lock: SerializedLock): Result[String, String] = {
    try {
      implicit val formats: org.json4s.Formats = serialization.formats
      Result.Ok(org.json4s.native.Serialization.write(lock))
    } catch {
      case e: Exception => Result.Err(s"Invalid AST: ${e.getMessage}")
    }
  }

  /**
    * Returns the lock `json` describes, without deserializing the schemes it holds.
    *
    * Used to keep the sections of the packages that are not being locked, so that locking one
    * package leaves the rest of the file as it was.
    */
  def parse(json: String): Result[SerializedLock, String] = {
    try {
      implicit val formats: org.json4s.Formats = serialization.formats
      Result.Ok(org.json4s.native.Serialization.read[SerializedLock](json))
    } catch {
      case e: Exception => Result.Err(s"Unexpected JSON: ${e.getMessage}")
    }
  }

  /**
    * Returns the schemes `json` locks, for each package it locks them for.
    */
  def deserialize(json: String): Result[Map[PackageId, LockedPackage], String] = {
    parse(json).flatMap { lock =>
      Result.traverse(lock) {
        case (id, entries) => PackageId.mkPackageId(id) match {
          case None => Result.Err(s"Not a package: '$id'")
          case Some(pkg) => Result.Ok(pkg -> mkLockedPackage(entries))
        }
      }.map(_.toMap)
    }
  }

  /**
    * Returns the schemes of `entries`, split into the defs and the sigs they belong to.
    */
  private def mkLockedPackage(entries: Map[String, serialization.DefOrSig]): LockedPackage = {
    val defs = entries.values.collect {
      case defn: serialization.SDef => Deserialize.deserializeDef(defn)
    }
    val sigs = entries.values.collect {
      case sig: serialization.SSig => Deserialize.deserializeSig(sig)
    }
    LockedPackage(defs.toMap, sigs.toMap)
  }

  /**
    * Returns the package `src` belongs to, if it belongs to one.
    *
    * A source of the user or of the bundled library belongs to no package.
    */
  private def packageOf(src: Source): Option[PackageId] = src.origin match {
    case Origin.User => None
    case Origin.Library => None
    case Origin.Package(id) => Some(id)
    case Origin.Unknown => None
  }

}
