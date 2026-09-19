/*
 * Copyright 2022 Paul Butcher, Lukas Rønn
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
package ca.uwaterloo.flix.api.lsp.provider.completion

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.lsp.{CompletionItemKind, Position, Range}
import ca.uwaterloo.flix.api.lsp.provider.completion.Completion.UseCompletion
import ca.uwaterloo.flix.api.lsp.provider.completion.CompletionUtils.fuzzyMatch
import ca.uwaterloo.flix.language.ast.shared.{Mountpoint, PackageId}
import ca.uwaterloo.flix.language.ast.{Name, SourceLocation, SourcePosition, Symbol, TypedAst}

object UseCompleter {

  /**
    * A package that a use is completed in.
    *
    * @param id    the package.
    * @param mount the mount the completion is written under, e.g. `flixball::Game.Board`. It is `None`
    *              for an item of a use many, which is written as its name alone.
    */
  private case class InPackage(id: PackageId, mount: Option[Mountpoint])

  /**
    * Returns a List of Completions for use clause.
    */
  def getCompletions(qn: Name.QName, range: Range)(implicit root: TypedAst.Root): Iterable[Completion] =
    getCompletions(qn, range, None)

  /**
    * Returns a List of Completions for a use clause that names the package `id`, mounted as `pkg`.
    *
    * @param loc the location of the use.
    */
  def getPackageCompletions(pkg: Name.Ident, id: PackageId, qn: Name.QName, loc: SourceLocation)(implicit root: TypedAst.Root): Iterable[Completion] = {
    // A path directly after the `::` is replaced together with its package, e.g. `flixball::Ga`.
    // An item of a use many is replaced on its own, e.g. `pl` in `use flixball::Game.{name, pl}`.
    val isWholePath = qn.loc.start == SourcePosition.moveRight(SourcePosition.moveRight(pkg.loc.end))
    if (isWholePath)
      getCompletions(qn, Range(Position.fromBegin(pkg.loc), Position.fromEnd(loc)), Some(InPackage(id, Some(Mountpoint(pkg.name)))))
    else
      getCompletions(qn, Range.from(loc), Some(InPackage(id, None)))
  }

  private def getCompletions(qn: Name.QName, range: Range, pkg: Option[InPackage])(implicit root: TypedAst.Root): Iterable[Completion] = {
    val namespace = namespaceOf(qn, pkg)
    val ident = qn.ident.name
    val moduleSym = Symbol.mkModuleSym(namespace)
    root.modules.get(moduleSym).map(_.children).getOrElse(Nil).collect {
      case mod: Symbol.ModuleSym if fuzzyMatch(ident, mod.ns.last) && isAvailable(mod, pkg) => UseCompletion(render(mod, pkg), range, Priority.Medium(0), CompletionItemKind.Module)
      case enm: Symbol.EnumSym if fuzzyMatch(ident, enm.name) && CompletionUtils.isAvailable(enm) => UseCompletion(render(enm, pkg), range, Priority.Medium(0), CompletionItemKind.Enum)
      case eff: Symbol.EffSym if fuzzyMatch(ident, eff.name) && CompletionUtils.isAvailable(eff) => UseCompletion(render(eff, pkg), range, Priority.Medium(0), CompletionItemKind.Event)
      case defn: Symbol.DefnSym if fuzzyMatch(ident, defn.name) && CompletionUtils.isAvailable(defn) => UseCompletion(render(defn, pkg), range, Priority.Medium(0), CompletionItemKind.Function)
      case trt: Symbol.TraitSym if fuzzyMatch(ident, trt.name) && CompletionUtils.isAvailable(trt) => UseCompletion(render(trt, pkg), range, Priority.Medium(0), CompletionItemKind.Interface)
    } ++ getSigCompletions(qn, range, pkg) ++ getOpCompletions(qn, range, pkg) ++ getTagCompletions(qn, range, pkg)
  }

  /**
    * Returns a List of Completions for the mounts of the project that match `prefix`, e.g. `flixball::` for `use fl`.
    *
    * The `::` is part of the completion unless it is already written, i.e. unless `separator` is `false`.
    */
  def getMountCompletions(prefix: String, range: Range, separator: Boolean)(implicit flix: Flix): Iterable[Completion] =
    flix.rootMounts.keys.toList.sorted.collect {
      case mount if fuzzyMatch(prefix, mount.name) =>
        UseCompletion(if (separator) s"$mount::" else mount.name, range, Priority.Medium(0), CompletionItemKind.Module)
    }

  /**
    * Returns the namespace `qn` is completed in: its own, under the root of `pkg` if it is given.
    */
  private def namespaceOf(qn: Name.QName, pkg: Option[InPackage]): List[String] =
    pkg.map(_.id.canonicalRoot).toList ::: qn.namespace.idents.map(_.name)

  /**
    * Returns `sym` as it is written in a use.
    *
    * A symbol of a package prints under the identifier of the package, which cannot be written, so
    * it is written under the mount instead, e.g. `flixball::Game.Board`.
    */
  private def render(sym: Symbol, pkg: Option[InPackage]): String = pkg match {
    case None => sym.toString
    case Some(InPackage(id, mount)) =>
      val path = sym.toString.stripPrefix(s"$id.")
      mount match {
        case Some(m) => s"$m::$path"
        case None => path.substring(path.lastIndexOf('.') + 1)
      }
  }

  /**
    * Returns `true` if the module `mod` can be used. A module of a package must be public.
    */
  private def isAvailable(mod: Symbol.ModuleSym, pkg: Option[InPackage])(implicit root: TypedAst.Root): Boolean =
    pkg.isEmpty || root.modules.get(mod).exists(_.mod.isPublic)

  /**
    * Returns a List of Completion for signatures.
    */
  private def getSigCompletions(qn: Name.QName, range: Range, pkg: Option[InPackage])(implicit root: TypedAst.Root): Iterable[Completion] = {
    val traitSym = Symbol.mkTraitSym(namespaceOf(qn, pkg).mkString("."))
    root.traits.get(traitSym).filter(CompletionUtils.isAvailable).map(_.sigs.collect {
      case sig if fuzzyMatch(qn.ident.name, sig.sym.name) && sig.spec.mod.isPublic =>
        UseCompletion(render(sig.sym, pkg), range, Priority.Medium(0), CompletionItemKind.Method)
    }).getOrElse(Nil)
  }

  /**
    * Returns a List of Completion for ops.
    */
  private def getOpCompletions(qn: Name.QName, range: Range, pkg: Option[InPackage])(implicit root: TypedAst.Root): Iterable[Completion] = {
    val effectSym = Symbol.mkEffSym(namespaceOf(qn, pkg).mkString("."))
    root.effects.get(effectSym).filter(CompletionUtils.isAvailable).map(_.ops.collect {
      case op if fuzzyMatch(qn.ident.name, op.sym.name) =>
        UseCompletion(render(op.sym, pkg), range, Priority.Medium(0), CompletionItemKind.Method)
    }).getOrElse(Nil)
  }

  /**
    * Returns a List of Completion for tags.
    */
  private def getTagCompletions(qn: Name.QName, range: Range, pkg: Option[InPackage])(implicit root: TypedAst.Root): Iterable[Completion] = {
    val enumSym = Symbol.mkEnumSym(namespaceOf(qn, pkg).mkString("."))
    root.enums.get(enumSym).filter(CompletionUtils.isAvailable).map(_.cases.values.collect {
      case tag if fuzzyMatch(qn.ident.name, tag.sym.name) =>
        UseCompletion(render(tag.sym, pkg), range, Priority.Medium(0), CompletionItemKind.EnumMember)
    }).getOrElse(Nil)
  }
}
