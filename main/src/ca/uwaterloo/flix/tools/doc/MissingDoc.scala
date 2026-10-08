/*
 * Copyright 2026 Flix Authors
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.tools.doc

import ca.uwaterloo.flix.language.ast.shared.{Doc, Origin}
import ca.uwaterloo.flix.language.ast.{SourceLocation, TypedAst}
import ca.uwaterloo.flix.tools.doc.HtmlDocumentor.{Effect, Enum, Module, Struct, Trait}
import ca.uwaterloo.flix.util.Formatter

/**
  * Reports the documentable items that have no doc comment:
  *
  * {{{
  * missing doc comment: module Foo (Foo.flix:1)
  * missing doc comment: def Foo.bar (Foo.flix:2)
  *
  * Found 2 item(s) with no doc comment.
  * }}}
  */
object MissingDoc {

  /**
    * Returns every item under `origin` in `root` that would appear in the documentation generated
    * by [[HtmlDocumentor.run]], but has no doc comment.
    */
  def check(root: TypedAst.Root, origin: Origin): List[MissingDoc] =
    checkMod(HtmlDocumentor.documentedModules(root, origin)).sortBy(m => (m.qualifiedName, m.kind))

  /**
    * Returns a human-readable report of `missing`, one grep-able line per item (e.g.
    * `missing doc comment: def Foo.bar (Foo.flix:12)`), followed by a summary count.
    */
  def format(missing: List[MissingDoc], f: Formatter): String = {
    if (missing.isEmpty) ""
    else {
      val lines = missing.map(_.format(f)).mkString(System.lineSeparator())

      s"""$lines
         |
         |Found ${f.red(missing.size.toString)} item(s) with no doc comment.
         |""".stripMargin
    }
  }

  /**
    * Returns the missing-doc-comment items of `mod`, its contents, and its submodules.
    *
    * The root module is a pseudo-module with no declaration of its own (it is never written to a
    * page), so its own doc comment, unlike that of every other module, is not checked.
    */
  private def checkMod(mod: Module): List[MissingDoc] = {
    val self = if (mod.sym.isRoot) Nil else checkModDoc(mod)
    self ++ checkContents(mod)
  }

  /**
    * Returns `List(MissingDoc("module", ...))` if `mod` has no doc comment, and `Nil` otherwise.
    *
    * A module that exists only implicitly -- e.g. `Foo.Bar` because of a declaration
    * `def Foo.Bar.f(): ...`, without any `mod Foo.Bar { ... }` block of its own -- has a synthetic
    * location (`!mod.doc.loc.isReal`) and no place for a doc comment to go, so it is skipped.
    * This should change once we start requiring all intermediate modules to exist.
    */
  private def checkModDoc(mod: Module): List[MissingDoc] =
    if (!mod.doc.loc.isReal) Nil else missingDoc("module", mod.qualifiedName, mod.doc)

  /**
    * Returns the missing-doc-comment items contained directly in `mod`: its submodules (and, in
    * turn, their contents), traits, effects, enums, structs, type aliases, and definitions.
    */
  private def checkContents(mod: Module): List[MissingDoc] = {
    mod.submodules.flatMap(checkMod) ++
      mod.traits.flatMap(checkTrait) ++
      mod.effects.flatMap(checkEffect) ++
      mod.enums.flatMap(checkEnum) ++
      mod.structs.flatMap(checkStruct) ++
      mod.typeAliases.flatMap(t => missingDoc("type alias", t.sym.toString, t.doc)) ++
      mod.defs.flatMap(d => missingDoc("def", d.sym.toString, d.spec.doc))
  }

  /**
    * Returns the missing-doc-comment items of `companionMod`, if any: its own doc comment
    * (rendered on the page of the trait/effect/enum/struct it belongs to) and its contents.
    */
  private def checkCompanionMod(companionMod: Option[Module]): List[MissingDoc] =
    companionMod.toList.flatMap(m => checkModDoc(m) ++ checkContents(m))

  /**
    * Returns the missing-doc-comment items of `trt`: the trait itself, its signatures and trait
    * definitions, and the contents of its companion module, if any.
    */
  private def checkTrait(trt: Trait): List[MissingDoc] =
    missingDoc("trait", trt.qualifiedName, trt.decl.doc) ++
      trt.signatures.flatMap(s => missingDoc("signature", s.sym.toString, s.spec.doc)) ++
      trt.defs.flatMap(d => missingDoc("trait def", d.sym.toString, d.spec.doc)) ++
      checkCompanionMod(trt.companionMod)

  /**
    * Returns the missing-doc-comment items of `eff`: the effect itself, its operations, and the
    * contents of its companion module, if any.
    */
  private def checkEffect(eff: Effect): List[MissingDoc] =
    missingDoc("effect", eff.qualifiedName, eff.decl.doc) ++
      eff.decl.ops.flatMap(o => missingDoc("effect operation", o.sym.toString, o.spec.doc)) ++
      checkCompanionMod(eff.companionMod)

  /**
    * Returns the missing-doc-comment items of `enm`: the enum itself and the contents of its
    * companion module, if any.
    */
  private def checkEnum(enm: Enum): List[MissingDoc] =
    missingDoc("enum", enm.qualifiedName, enm.decl.doc) ++
      checkCompanionMod(enm.companionMod)

  /**
    * Returns the missing-doc-comment items of `struct`: the struct itself and the contents of its
    * companion module, if any.
    */
  private def checkStruct(struct: Struct): List[MissingDoc] =
    missingDoc("struct", struct.qualifiedName, struct.decl.doc) ++
      checkCompanionMod(struct.companionMod)

  /**
    * Returns `List(MissingDoc(kind, qualifiedName, doc.loc))` if `doc` is empty, i.e. the item it
    * documents has no doc comment, and `Nil` otherwise.
    */
  private def missingDoc(kind: String, qualifiedName: String, doc: Doc): List[MissingDoc] =
    if (doc.text.isEmpty) List(MissingDoc(kind, qualifiedName, doc.loc)) else Nil

}

/**
  * A documentable item that has no doc comment.
  *
  * @param kind          a short, human-readable description of the kind of item, e.g. `"module"` or `"def"`.
  * @param qualifiedName the fully qualified name of the item.
  * @param loc           the source location of the item.
  */
case class MissingDoc(kind: String, qualifiedName: String, loc: SourceLocation) {

  /**
    * Returns the line of the report for this item, e.g. `missing doc comment: def Foo.bar (Foo.flix:12)`.
    */
  def format(f: Formatter): String =
    s"${f.red("missing doc comment")}: $kind ${f.bold(qualifiedName)} (${loc.source.name}:${loc.startLine})"

}
