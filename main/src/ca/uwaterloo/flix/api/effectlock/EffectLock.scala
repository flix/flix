/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.{Origin, PackageId, Source}
import ca.uwaterloo.flix.language.ast.{Name, Symbol, TypedAst}
import ca.uwaterloo.flix.util.Sha256

object EffectLock {

  /**
    * The outcome of checking a lock file against a program.
    *
    * @param compared how many locked signatures were compared, which is how many of the locked
    *                 declarations the packages still declare publicly.
    * @param changed  the declarations whose signature is no longer the one that was locked: the
    *                 package, the name of the declaration inside it, and the spec it is declared
    *                 with now.
    */
  case class CheckResult(compared: Int, changed: List[(PackageId, String, TypedAst.Spec)])

  /**
    * The public declarations of one package, each by its name inside the package.
    */
  private case class Declarations(defs: Map[String, TypedAst.Spec], sigs: Map[String, TypedAst.Spec])

  private object Declarations {
    /** The declarations of a package that declares nothing publicly. */
    val Empty: Declarations = Declarations(Map.empty, Map.empty)
  }

  /**
    * Returns the hashes of the signatures of the public defs and sigs of `targets` in `root`.
    *
    * A declaration is recorded by its name inside its package, e.g. `Museum.Clerk.buy`: the entry
    * it is recorded under already names the package, so the name does not.
    *
    * Only a package can be locked. The library bundled with the compiler cannot change under a
    * project, because the version of Flix that provides it is fixed by the manifest, so it has no
    * lock of its own.
    */
  def lock(root: TypedAst.Root, targets: Set[PackageId]): EffectLockfile =
    EffectLockfile(declarationsOf(root, targets).map {
      case (id, decls) => id -> LockedPackage(hashesOf(decls.defs), hashesOf(decls.sigs))
    })

  /**
    * Returns the outcome of checking `lockfile` against the declarations of `targets` in `root`:
    * how many locked signatures were compared, and the declarations that `lockfile` locks at a
    * signature they no longer have, each with the spec it is declared with now.
    *
    * A locked declaration is compared against the public declaration of the same name in the
    * package it is locked under, and against nothing else. One that the package no longer
    * declares, or no longer declares publicly, is not compared, and that is not an error: it
    * cannot be called, so it cannot do anything the lock did not allow. The count of what was
    * compared is returned so that a check that matched nothing can be told from one that found
    * nothing changed.
    *
    * A package that `lockfile` locks but that `targets` does not name is not checked, which is
    * how one package is checked without the drift of another standing in the way.
    *
    * The hash says whether a signature is the one that was locked, and nothing more. A change
    * that only narrows what a declaration may do is reported like any other: telling the two
    * apart would mean holding the signature that was locked, which the lock file does not.
    */
  def check(lockfile: EffectLockfile, root: TypedAst.Root, targets: Set[PackageId]): CheckResult = {
    val declared = declarationsOf(root, targets)
    val results = lockfile.packages.toList.filter { case (id, _) => targets.contains(id) }.sortBy { case (id, _) => id }.map {
      case (id, locked) =>
        val decls = declared.get(id) match {
          case None => Declarations.Empty
          case Some(d) => d
        }
        val (comparedDefs, changedDefs) = compare(locked.defs, decls.defs)
        val (comparedSigs, changedSigs) = compare(locked.sigs, decls.sigs)
        val changed = (changedDefs ::: changedSigs).map { case (name, spec) => (id, name, spec) }
        (comparedDefs + comparedSigs, changed)
    }
    CheckResult(results.map { case (compared, _) => compared }.sum, results.flatMap { case (_, changed) => changed })
  }

  /**
    * Returns how many of `locked` are declared in `current`, and those of them whose signature
    * hashes to something other than what was locked, each with the spec it is declared with now,
    * in order of name.
    */
  private def compare(locked: Map[String, Sha256], current: Map[String, TypedAst.Spec]): (Int, List[(String, TypedAst.Spec)]) = {
    val declared = locked.toList.sortBy { case (name, _) => name }.flatMap {
      case (name, hash) => current.get(name) match {
        case None => Nil
        case Some(spec) => List((name, hash, spec))
      }
    }
    val changed = declared.collect {
      case (name, hash, spec) if HashType.hashScheme(spec.declaredScheme) != hash => (name, spec)
    }
    (declared.length, changed)
  }

  /**
    * Returns the public declarations of each of `targets` in `root`, each by its name inside its
    * package. A package that declares nothing publicly has no entry.
    */
  private def declarationsOf(root: TypedAst.Root, targets: Set[PackageId]): Map[PackageId, Declarations] = {
    val defs = groupByPackage(root.defs.toList.map { case (sym, defn) => (sym.src, nameOf(sym), defn.spec) }, targets)
    val sigs = groupByPackage(root.sigs.toList.map { case (sym, sig) => (sym.src, nameOf(sym), sig.spec) }, targets)
    (defs.keySet ++ sigs.keySet).toList.map {
      id => id -> Declarations(defs.getOrElse(id, Map.empty), sigs.getOrElse(id, Map.empty))
    }.toMap
  }

  /**
    * Returns the public declarations of `decls` that belong to one of `targets`, grouped by the
    * package each belongs to and keyed by name.
    */
  private def groupByPackage(decls: List[(Source, String, TypedAst.Spec)], targets: Set[PackageId]): Map[PackageId, Map[String, TypedAst.Spec]] = {
    decls.flatMap {
      case (src, name, spec) => packageOf(src) match {
        case Some(id) if spec.mod.isPublic && targets.contains(id) => List(id -> (name -> spec))
        case Some(_) => Nil
        case None => Nil
      }
    }.groupMap {
      case (id, _) => id
    } {
      case (_, entry) => entry
    }.map {
      case (id, entries) => id -> entries.toMap
    }
  }

  /**
    * Returns the hash of the signature of each of `decls`.
    */
  private def hashesOf(decls: Map[String, TypedAst.Spec]): Map[String, Sha256] =
    decls.map { case (name, spec) => name -> HashType.hashScheme(spec.declaredScheme) }

  /**
    * Returns the name of `sym` inside its package: the modules it is declared in, without the
    * root of the package, and its own name, joined by dots.
    */
  private def nameOf(sym: Symbol.DefnSym): String =
    (Name.declaredParts(sym.namespace) :+ sym.text).mkString(".")

  /**
    * Returns the name of `sym` inside its package: the modules its trait is declared in, without
    * the root of the package, then the trait, then its own name, joined by dots.
    */
  private def nameOf(sym: Symbol.SigSym): String =
    (Name.declaredParts(sym.trt.namespace) :+ sym.trt.name :+ sym.name).mkString(".")

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
