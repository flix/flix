/*
 * Copyright 2025 Jakob Schneider Villumsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */
package ca.uwaterloo.flix.api.effectlock

import ca.uwaterloo.flix.language.ast.shared.{Origin, PackageId, QualifiedSym, Source}
import ca.uwaterloo.flix.language.ast.{Name, TypedAst}
import ca.uwaterloo.flix.util.Sha256

object EffectLock {

  /**
    * The outcome of checking a lock file against a program.
    *
    * @param compared how many locked signatures were compared, which is how many of the locked
    *                 declarations the packages still declare publicly.
    * @param changed  the declarations whose signature is no longer the one that was locked: the
    *                 package, the symbol, and the spec it is declared with now.
    */
  case class CheckResult(compared: Int, changed: List[(PackageId, QualifiedSym, TypedAst.Spec)])

  /**
    * The public declarations of one package, each by its symbol.
    */
  private case class Declarations(defs: Map[QualifiedSym, TypedAst.Spec], sigs: Map[QualifiedSym, TypedAst.Spec])

  private object Declarations {
    /** The declarations of a package that declares nothing publicly. */
    val Empty: Declarations = Declarations(Map.empty, Map.empty)
  }

  /**
    * Returns the hashes of the signatures of the public defs and sigs of `targets` in `root`.
    *
    * A declaration is recorded by the name of [[nameOf]], e.g. `Museum.Clerk.buy`.
    *
    * `targets` are the dependencies the project declares, and not the packages reached through
    * them.
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
    * package it is locked under, and against nothing else, whether it was locked as a def or as
    * a sig: a def and a sig cannot share a name, so a locked def that the package now declares
    * as a sig of that name is compared against the sig, and the reverse. One that the package
    * no longer declares, or no longer declares publicly, is not compared, and that is not an
    * error: it cannot be called, so it cannot do anything the lock did not allow. The count of
    * what was compared is returned so that a check that matched nothing can be told from one
    * that found nothing changed.
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
        // The defs and the sigs are compared together, so that a name locked as one kind and
        // declared as the other is compared rather than passed over. Neither side holds a name
        // twice: a def and a sig cannot share a name, and the declarations are keyed by symbol.
        val (compared, changed) = compare(locked.defs ++ locked.sigs, decls.defs ++ decls.sigs)
        (compared, changed.map { case (sym, spec) => (id, sym, spec) })
    }
    CheckResult(results.map { case (compared, _) => compared }.sum, results.flatMap { case (_, changed) => changed })
  }

  /**
    * Returns the packages of `targets` that declare something publicly in `root` but that
    * `lockfile` does not lock, in order of identifier.
    *
    * A package that declares nothing publicly is not reported, whether it is locked or not:
    * nothing in it can be called, so there is nothing for a lock to vouch for. It has no entry in
    * a lock file either, see [[lock]], so having no entry is not by itself what tells an unlocked
    * package apart.
    */
  def unlocked(lockfile: EffectLockfile, root: TypedAst.Root, targets: Set[PackageId]): List[PackageId] =
    declarationsOf(root, targets).keySet.diff(lockfile.packages.keySet).toList.sorted

  /**
    * Returns the name that a lock file records `sym` under: its namespace without the root of
    * its package, and then its own name, joined by dots, e.g. `Museum.Clerk.buy`.
    *
    * The entry a signature is recorded under names the package, so the name does not. The
    * namespace of a sig ends with its trait, so a sig is named under the trait.
    */
  def nameOf(sym: QualifiedSym): String = (Name.declaredParts(sym.namespace) :+ sym.name).mkString(".")

  /**
    * Returns how many of `current` are locked by `locked`, and those of them whose signature
    * hashes to something other than what was locked, each with the spec it is declared with now,
    * in order of name.
    */
  private def compare(locked: Map[String, Sha256], current: Map[QualifiedSym, TypedAst.Spec]): (Int, List[(QualifiedSym, TypedAst.Spec)]) = {
    val declared = current.toList.flatMap {
      case (sym, spec) => locked.get(nameOf(sym)) match {
        case None => Nil
        case Some(hash) => List((sym, hash, spec))
      }
    }
    val changed = declared.collect {
      case (sym, hash, spec) if HashType.hashScheme(spec.declaredScheme) != hash => (sym, spec)
    }
    (declared.length, changed.sortBy { case (sym, _) => nameOf(sym) })
  }

  /**
    * Returns the public declarations of each of `targets` in `root`, each by its symbol. A
    * package that declares nothing publicly has no entry.
    */
  private def declarationsOf(root: TypedAst.Root, targets: Set[PackageId]): Map[PackageId, Declarations] = {
    val defs = groupByPackage(root.defs.toList.map { case (sym, defn) => (sym.src, sym, defn.spec) }, targets)
    val sigs = groupByPackage(root.sigs.toList.map { case (sym, sig) => (sym.src, sym, sig.spec) }, targets)
    (defs.keySet ++ sigs.keySet).toList.map {
      id => id -> Declarations(defs.getOrElse(id, Map.empty), sigs.getOrElse(id, Map.empty))
    }.toMap
  }

  /**
    * Returns the public declarations of `decls` that belong to one of `targets`, grouped by the
    * package each belongs to and keyed by symbol.
    */
  private def groupByPackage(decls: List[(Source, QualifiedSym, TypedAst.Spec)], targets: Set[PackageId]): Map[PackageId, Map[QualifiedSym, TypedAst.Spec]] = {
    decls.flatMap {
      case (src, sym, spec) => packageOf(src) match {
        case Some(id) if spec.mod.isPublic && targets.contains(id) => List(id -> (sym -> spec))
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
    * Returns the hash of the signature of each of `decls`, by the name of [[nameOf]].
    */
  private def hashesOf(decls: Map[QualifiedSym, TypedAst.Spec]): Map[String, Sha256] =
    decls.map { case (sym, spec) => nameOf(sym) -> HashType.hashScheme(spec.declaredScheme) }

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
