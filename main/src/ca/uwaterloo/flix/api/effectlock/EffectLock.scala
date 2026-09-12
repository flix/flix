/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.{Origin, PackageId, Source}
import ca.uwaterloo.flix.language.ast.{Scheme, TypedAst}
import ca.uwaterloo.flix.util.Sha256

object EffectLock {

  /**
    * Returns the hashes of the signatures of the public defs and sigs of `targets` in `root`.
    *
    * Only a package can be locked. The library bundled with the compiler cannot change under a
    * project, because the version of Flix that provides it is fixed by the manifest, so it has no
    * lock of its own.
    */
  def lock(root: TypedAst.Root, targets: Set[PackageId]): EffectLockfile = {
    val defs = hashesOf(root.defs.toList.collect {
      case (sym, defn) if defn.spec.mod.isPublic => (sym.src, sym.toString, defn.spec.declaredScheme)
    }, targets)
    val sigs = hashesOf(root.sigs.toList.collect {
      case (sym, sig) if sig.spec.mod.isPublic => (sym.src, sym.toString, sig.spec.declaredScheme)
    }, targets)

    val ids = defs.keySet ++ sigs.keySet
    EffectLockfile(ids.map {
      id => id -> LockedPackage(defs.getOrElse(id, Map.empty), sigs.getOrElse(id, Map.empty))
    }.toMap)
  }

  /**
    * Returns the declarations of `targets` in `root` that `lockfile` locks at a signature they no
    * longer have, each with the spec it is declared with now.
    *
    * A package that `lockfile` locks but that `targets` does not name is not checked, which is
    * how one package is checked without the drift of another standing in the way.
    *
    * The hash says whether a signature is the one that was locked, and nothing more. A change
    * that only narrows what a declaration may do is reported like any other: telling the two
    * apart would mean holding the signature that was locked, which the lock file does not.
    */
  def check(lockfile: EffectLockfile, root: TypedAst.Root, targets: Set[PackageId]): List[(PackageId, String, TypedAst.Spec)] = {
    val defs = root.defs.map { case (sym, defn) => sym.toString -> defn.spec }
    val sigs = root.sigs.map { case (sym, sig) => sym.toString -> sig.spec }

    lockfile.packages.toList.filter { case (id, _) => targets.contains(id) }.sortBy { case (id, _) => id }.flatMap {
      case (id, locked) =>
        val changes = changedSignatures(locked.defs, defs) ::: changedSignatures(locked.sigs, sigs)
        changes.map { case (sym, spec) => (id, sym, spec) }
    }
  }

  /**
    * Returns the symbols of `locked` that `current` declares with a signature that hashes to
    * something other than what was locked, each with the spec it is declared with now.
    *
    * A locked declaration that the program no longer has is not an error: it cannot be called,
    * so it cannot do anything the lock did not allow.
    */
  private def changedSignatures(locked: Map[String, Sha256], current: Map[String, TypedAst.Spec]): List[(String, TypedAst.Spec)] = {
    locked.toList.sortBy { case (sym, _) => sym }.flatMap {
      case (sym, hash) => current.get(sym).filter(spec => HashType.hashScheme(spec.declaredScheme) != hash).map(spec => (sym, spec))
    }
  }

  /**
    * Returns the hash of the signature of each declaration of `decls` that belongs to one of
    * `targets`, grouped by the package it belongs to.
    */
  private def hashesOf(decls: List[(Source, String, Scheme)], targets: Set[PackageId]): Map[PackageId, Map[String, Sha256]] = {
    decls.flatMap {
      case (src, sym, sc) => packageOf(src).filter(targets.contains).map(id => id -> (sym -> HashType.hashScheme(sc)))
    }.groupMap {
      case (id, _) => id
    } {
      case (_, entry) => entry
    }.map {
      case (id, entries) => id -> entries.toMap
    }
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
