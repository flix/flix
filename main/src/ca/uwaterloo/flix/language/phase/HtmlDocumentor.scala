/*
 * Copyright 2023 Holger Dal Mogensen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.{Ast, Kind, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatType, SimpleType}
import ca.uwaterloo.flix.tools.pkg.PackageModules
import ca.uwaterloo.flix.util.LocalResource

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import com.github.rjeschke.txtmark

import java.net.URLEncoder
import scala.annotation.tailrec

/** A phase that emits a JSON file for library documentation. */
object HtmlDocumentor {

  /** The "Pseudo-name" of the root namespace displayed on the pages. */
  val RootNS: String = "Prelude"
  /** The "Pseudo-name" of the root namespace used for its file name. */
  val RootFileName: String = "index"

  /** The directory where to write the ouput. */
  val OutputDirectory: Path = Paths.get("./build/doc")

  /** The path to the the stylesheet, relative to the resources folder. */
  val Stylesheet: String = "/doc/styles.css"

  /** The path to the the favicon, relative to the resources folder. */
  val FavIcon: String = "/doc/favicon.png"

  /** The path to the the script, relative to the resources folder. */
  val Script: String = "/doc/index.js"

  /** The path to the the icon directory, relative to the resources folder. */
  val Icons: String = "/doc/icons"

  /** The root of the link to each file of the standard library. */
  val LibraryGitHub: String = "https://github.com/flix/flix/blob/master/main/src/library/"

  def run(root: TypedAst.Root, packageModules: PackageModules)(implicit flix: Flix): Unit = {
    val modulesRoot = splitModules(root)
    val filteredModulesRoot = filterModules(modulesRoot, packageModules)
    val pairedModulesRoot = pairModules(filteredModulesRoot)

    visitMod(pairedModulesRoot)
    writeAssets()
  }

  /** Documents the given `Module`, `mod`, and all of its contained items, writing the resulting HTML to disk. */
  private def visitMod(mod: Module)(implicit flix: Flix): Unit = {
    val out = documentModule(mod)
    writeDocFile(mod.fileName, out)

    mod.submodules.foreach(visitMod)
    mod.traits.foreach(visitTrait)
    mod.effects.foreach(visitEffect)
    mod.enums.foreach(visitEnum)
  }

  /** Documents the given `Trait`, `trt`, and all of its contained items, writing the resulting HTML to disk. */
  private def visitTrait(trt: Trait)(implicit flix: Flix): Unit = {
    val out = documentTrait(trt)
    writeDocFile(trt.fileName, out)

    trt.companionMod.foreach { mod =>
      mod.submodules.foreach(visitMod)
      mod.traits.foreach(visitTrait)
      mod.effects.foreach(visitEffect)
      mod.enums.foreach(visitEnum)
    }
  }

  /** Documents the given `Effect`, `eff`, and all of its contained items, writing the resulting HTML to disk. */
  private def visitEffect(eff: Effect)(implicit flix: Flix): Unit = {
    val out = documentEffect(eff)
    writeDocFile(eff.fileName, out)

    eff.companionMod.foreach { mod =>
      mod.submodules.foreach(visitMod)
      mod.traits.foreach(visitTrait)
      mod.effects.foreach(visitEffect)
      mod.enums.foreach(visitEnum)
    }
  }

  /** Documents the given `Enum`, `enm`, and all of its contained items, writing the resulting HTML to disk. */
  private def visitEnum(enm: Enum)(implicit flix: Flix): Unit = {
    val out = documentEnum(enm)
    writeDocFile(enm.fileName, out)

    enm.companionMod.foreach { mod =>
      mod.submodules.foreach(visitMod)
      mod.traits.foreach(visitTrait)
      mod.effects.foreach(visitEffect)
      mod.enums.foreach(visitEnum)
    }
  }

  /** Get the shortest name of the module symbol, e.g. 'StdOut'. */
  private def moduleName(sym: Symbol.ModuleSym): String = sym.ns.lastOption.getOrElse(RootNS)

  /** Get the fully qualified name of the module symbol, e.g. 'System.StdOut'. */
  private def moduleQualifiedName(sym: Symbol.ModuleSym): String = if (sym.isRoot) RootNS else sym.toString

  /** Get the file name of the module symbol, e.g. 'System.StdOut.html'. */
  private def moduleFileName(sym: Symbol.ModuleSym): String = s"${if (sym.isRoot) RootFileName else sym.toString}.html"

  /** Get the shortest name of the trait symbol, e.g. 'Foldable'. */
  private def traitName(sym: Symbol.TraitSym): String = sym.name

  /** Get the fully qualified name of the trait symbol, e.g. 'Fixpoint.PredSymsOf'. */
  private def traitQualifiedName(sym: Symbol.TraitSym): String = sym.toString

  /** Get the file name of the trait symbol, e.g. 'Fixpoint.PredSymsOf.html'. */
  private def traitFileName(sym: Symbol.TraitSym): String = s"${sym.toString}.html"

  /** Get the shortest name of the effect symbol, e.g. 'StdOut'. */
  private def effectName(sym: Symbol.EffectSym): String = sym.name

  /** Get the fully qualified name of the effect symbol, e.g. 'System.StdOut'. */
  private def effectQualifiedName(sym: Symbol.EffectSym): String = sym.toString

  /** Get the file name of the effect symbol, e.g. 'System.StdOut.html'. */
  private def effectFileName(sym: Symbol.EffectSym): String = s"${sym.toString}.html"

  /** Get the shortest name of the enum symbol, e.g. 'StdOut'. */
  private def enumName(sym: Symbol.EnumSym): String = sym.name

  /** Get the fully qualified name of the enum symbol, e.g. 'System.StdOut'. */
  private def enumQualifiedName(sym: Symbol.EnumSym): String = sym.toString

  /** Get the file name of the enum symbol, e.g. 'System.StdOut.html'. */
  private def enumFileName(sym: Symbol.EnumSym): String = s"${sym.toString}.html"

  /**
    * Splits the modules present in the root into a tree of `HtmlDocumentor.Module`s, making them easier to work with.
    *
    * Note: This function leaves all companion module fields empty.
    * Use `pairModules` to fill them in.
    */
  private def splitModules(root: TypedAst.Root): Module = {

    /** Visits a module and all of its submodules */
    def visitMod(moduleSym: Symbol.ModuleSym, parent: Option[Symbol.ModuleSym]): Module = {
      val mod = root.modules(moduleSym)
      val uses = root.uses.getOrElse(moduleSym, Nil)

      var submodules: List[Symbol.ModuleSym] = Nil
      var traits: List[Trait] = Nil
      var effects: List[Effect] = Nil
      var enums: List[Enum] = Nil
      var typeAliases: List[TypedAst.TypeAlias] = Nil
      var defs: List[TypedAst.Def] = Nil
      mod.foreach {
        case sym: Symbol.ModuleSym => submodules = sym :: submodules
        case sym: Symbol.TraitSym =>
          traits = mkTrait(sym, moduleSym, root) :: traits
        case sym: Symbol.EffectSym =>
          effects = mkEffect(sym, moduleSym, root) :: effects
        case sym: Symbol.EnumSym =>
          enums = mkEnum(sym, moduleSym, root) :: enums
        case sym: Symbol.TypeAliasSym => typeAliases = root.typeAliases(sym) :: typeAliases
        case sym: Symbol.DefnSym => defs = root.defs(sym) :: defs
        case _ => // No op
      }

      Module(
        moduleSym,
        parent,
        uses,
        submodules.map(visitMod(_, Some(moduleSym))),
        traits,
        effects,
        enums,
        typeAliases,
        defs,
      )
    }

    visitMod(Symbol.mkModuleSym(Nil), None)
  }

  /**
    * Extracts all relevant information about the given `TraitSym` from the root, into a `HtmlDocumentor.Trait`,
    * leaving the companion module unpopulated.
    */
  private def mkTrait(sym: Symbol.TraitSym, parent: Symbol.ModuleSym, root: TypedAst.Root): Trait = {
    val decl = root.traits(sym)

    val (sigs, defs) = decl.sigs.partition(_.exp.isEmpty)
    val instances = root.instances.getOrElse(sym, Nil)

    Trait(decl, sigs, defs, instances, parent, None)
  }

  /**
    * Extracts all relevant information about the given `EffectSym` from the root, into a `HtmlDocumentor.Effect`,
    * * leaving the companion module unpopulated.
    */
  private def mkEffect(sym: Symbol.EffectSym, parent: Symbol.ModuleSym, root: TypedAst.Root): Effect = {
    Effect(root.effects(sym), parent, None)
  }

  /**
    * Extracts all relevant information about the given `EnumSym` from the root, into a `HtmlDocumentor.Enum`,
    * * leaving the companion module unpopulated.
    */
  private def mkEnum(sym: Symbol.EnumSym, parent: Symbol.ModuleSym, root: TypedAst.Root): Enum = {

    /** Checks if a [[TypedAst.Instance]] with the given type `tpe` should be included on the page of the given enum. */
    @tailrec
    def enumMatchesInstance(enm: Symbol.EnumSym, tpe: Type): Boolean = tpe match {
      // An instance should be included if:
      // 1. An instance exists directly, e.g. `Eq[Boxed]`
      case Type.Cst(TypeConstructor.Enum(s, _), _) => enm == s
      // 2. An instance exists, consisting of the enum having been applied with some number of parameters, e.g. `Eq[Chain[a]] with Eq[a]`
      case Type.Apply(t, _, _) => enumMatchesInstance(enm, t)
      // Othwerwise not
      case _ => false
    }

    val allInstances = root.instances.values.flatten
    val instances = allInstances.filter(i => enumMatchesInstance(sym, i.tpe)).toList

    Enum(root.enums(sym), instances, parent, None)
  }

  /** Filter the module, `mod`, and its children, removing all items and empty modules, which shouldn't appear in the documentation. */
  private def filterModules(mod: Module, packageModules: PackageModules): Module = {
    filterEmpty(filterContents(mod, packageModules))
  }

  /**
    * Returns a tree of modules corresponding to the given input,
    * but with all contained items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterContents(mod: Module, packageModules: PackageModules): Module = mod match {
    case Module(sym, parent, uses, submodules, traits, effects, enums, typeAliases, defs) =>
      val included = packageModules.contains(sym)
      if (included) {
        Module(
          sym,
          parent,
          uses,
          submodules.map(m => filterContents(m, PackageModules.All)),
          traits.filter(c => c.decl.mod.isPublic && !c.decl.ann.isInternal).map(c => filterTrait(c)),
          effects.filter(e => e.decl.mod.isPublic && !e.decl.ann.isInternal).map(e => filterEffect(e)),
          enums.filter(e => e.decl.mod.isPublic && !e.decl.ann.isInternal).map(e => filterEnum(e)),
          typeAliases.filter(t => t.mod.isPublic && !t.ann.isInternal),
          defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
        )
      } else {
        // Keep the 'spine' of the tree if a module further down is included
        val sm = submodules ++
          traits.flatMap(c => c.companionMod) ++
          effects.flatMap(e => e.companionMod) ++
          enums.flatMap(e => e.companionMod)

        Module(
          sym,
          parent,
          Nil,
          sm.map(m => filterContents(m, packageModules)),
          Nil,
          Nil,
          Nil,
          Nil,
          Nil,
        )
      }
  }

  /**
    * Returns a `Trait` corresponding to the given `trt`,
    * but with all items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterTrait(trt: Trait): Trait = trt match {
    case Trait(TypedAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, _, laws, loc), signatures, defs, instances, parent, _) =>
      Trait(
        TypedAst.Trait(
          doc,
          ann,
          mod,
          sym,
          tparam,
          superTraits,
          assocs,
          Nil,
          laws.filter(l => l.spec.mod.isPublic && !l.spec.ann.isInternal),
          loc
        ),
        signatures.filter(s => s.spec.mod.isPublic && !s.spec.ann.isInternal),
        defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
        instances.filter(i => !i.ann.isInternal),
        parent,
        None
      )
  }


  /**
    * Returns an `Effect` corresponding to the given `eff`,
    * but with all items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterEffect(eff: Effect): Effect = eff match {
    case Effect(eff, parent, _) =>
      Effect(
        eff,
        parent,
        None,
      )
  }

  /**
    * Returns an `Enum` corresponding to the given `enm`,
    * but with all items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterEnum(enm: Enum): Enum = enm match {
    case Enum(enm, instances, parent, _) =>
      Enum(
        enm,
        instances,
        parent,
        None,
      )
  }

  /**
    * Remove any modules and references to them if they:
    *   1. Contain no items
    *   1. Contain no submodules with any items
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterEmpty(mod: Module): Module = {
    /** Recursively walks the module tree removing empty modules. */
    def visitMod(mod: Module): Option[Module] = mod match {
      case Module(sym, parent, uses, submodules, traits, effects, enums, typeAliases, defs) =>
        val filteredSubMods = submodules.flatMap(visitMod)

        val isEmpty =
          filteredSubMods.isEmpty &&
            traits.isEmpty &&
            effects.isEmpty &&
            enums.isEmpty &&
            typeAliases.isEmpty &&
            defs.isEmpty

        if (isEmpty) None
        else Some(
          Module(
            sym,
            parent,
            uses,
            filteredSubMods,
            traits,
            effects,
            enums,
            typeAliases,
            defs
          )
        )
    }

    visitMod(mod)
      .getOrElse(Module(
        mod.sym,
        None,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
      ))
  }

  /** Get the given module tree, but with all companion modules paired to their respective items. */
  private def pairModules(mod: Module): Module = mod match {
    case Module(sym, parent, uses, submodules, traits, effects, enums, typeAliases, defs) =>

      val visitedSubmodules = submodules.map(pairModules)

      /** Modules that should not be included as a submodule */
      var companionMods: List[Module] = Nil

      val pairedTraits = traits.map { t =>
        val comp = visitedSubmodules.find(m => m.sym.ns.last == t.decl.sym.name)
        comp.foreach(c => companionMods = c :: companionMods)
        t.copy(companionMod = comp)
      }
      val pairedEffects = effects.map { e =>
        val comp = visitedSubmodules.find(m => m.sym.ns.last == e.decl.sym.name)
        comp.foreach(c => companionMods = c :: companionMods)
        e.copy(companionMod = comp)
      }
      val pairedEnums = enums.map { e =>
        val comp = visitedSubmodules.find(m => m.sym.ns.last == e.decl.sym.name)
        comp.foreach(c => companionMods = c :: companionMods)
        e.copy(companionMod = comp)
      }

      val filteredSubmodules = visitedSubmodules.filterNot(companionMods.contains)

      Module(
        sym,
        parent,
        uses,
        filteredSubmodules,
        pairedTraits,
        pairedEffects,
        pairedEnums,
        typeAliases,
        defs,
      )
  }

  /** Documents the given `Module`, `mod`, returning a string of HTML. */
  private def documentModule(mod: Module)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedTraits = mod.traits.sortBy(_.name)
    val sortedEnums = mod.enums.sortBy(_.name)
    val sortedEffs = mod.effects.sortBy(_.name)
    val sortedTypeAliases = mod.typeAliases.sortBy(_.sym.name)
    val sortedDefs = mod.defs.sortBy(_.sym.name)

    sb.append(mkHead(mod.qualifiedName))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar(mod.parent) { () =>
      docSubModules(mod)
      docSideBarSection(
        "Traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        sortedDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main>")
    sb.append(s"<h1>${esc(mod.qualifiedName)}</h1>")
    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedDefs, docDef)
    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /** Documents the given `Trait`, `trt`, returning a string of HTML. */
  private def documentTrait(trt: Trait)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedAssocs = trt.decl.assocs.sortBy(_.sym.name)
    val sortedInstances = trt.instances.sortBy(_.loc)
    val sortedSigs = trt.signatures.sortBy(_.sym.name)
    val sortedTraitDefs = trt.defs.sortBy(_.sym.name)

    val mod = trt.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(trt.qualifiedName))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar(Some(trt.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Signatures",
        sortedSigs,
        (s: TypedAst.Sig) => sb.append(s"<a href='#sig-${escUrl(s.sym.name)}'>${esc(s.sym.name)}</a>"),
      )
      docSideBarSection(
        "Trait Definitions",
        sortedTraitDefs,
        (d: TypedAst.Sig) => sb.append(s"<a href='#sig-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
      docSideBarSection(
        "Traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Module Definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main>")
    sb.append(s"<h1>${esc(trt.qualifiedName)}</h1>")

    sb.append(s"<div class='box'>")
    docAnnotations(trt.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>trait</span> ")
    sb.append(s"<span class='name'>${esc(trt.name)}</span>")
    docTypeParams(List(trt.decl.tparam))
    docTraitConstraints(trt.decl.superTraits)
    sb.append("</code>")
    docActions(None, trt.decl.loc)
    sb.append("</div>")
    docDoc(trt.decl.doc)
    docSubSection("Associated Types", sortedAssocs, docAssoc)
    docCollapsableSubSection("Instances", sortedInstances, docInstance)
    sb.append("</div>")

    docSection("Signatures", sortedSigs, docSignature)
    docSection("Trait Definitions", sortedTraitDefs, docSignature)

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Module Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /** Documents the given `Effect`, `eff`, returning a string of HTML. */
  private def documentEffect(eff: Effect)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedOps = eff.decl.ops.sortBy(_.sym.name)

    val mod = eff.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(eff.qualifiedName))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar(Some(eff.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Operations",
        sortedOps, (o: TypedAst.Op) => sb.append(s"<a href='#op-${escUrl(esc(o.sym.name))}'>${esc(o.sym.name)}</a>")
      )
      docSideBarSection(
        "Traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main>")
    sb.append(s"<h1>${esc(eff.qualifiedName)}</h1>")

    sb.append(s"<div class='box' id='eff-${esc(eff.name)}'>")
    docAnnotations(eff.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>eff</span> ")
    sb.append(s"<span class='name'>${esc(eff.name)}</span>")
    sb.append("</code>")
    docActions(None, eff.decl.loc)
    sb.append("</div>")
    docDoc(eff.decl.doc)
    sb.append("</div>")

    docSection("Operations", sortedOps, docOp)

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /** Documents the given `Enum`, `enm`, returning a string of HTML. */
  private def documentEnum(enm: Enum)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedInstances = enm.instances.sortBy(_.trt.sym.name)

    val mod = enm.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(enm.qualifiedName))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar(Some(enm.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main>")
    sb.append(s"<h1>${esc(enm.qualifiedName)}</h1>")

    sb.append(s"<div class='box' id='enum-${esc(enm.name)}'>")
    docAnnotations(enm.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>enum</span> ")
    sb.append(s"<span class='name'>${esc(enm.name)}</span>")
    docTypeParams(enm.decl.tparams)
    docDerivations(enm.decl.derives)
    sb.append("</code>")
    docActions(None, enm.decl.loc)
    sb.append("</div>")
    docCases(enm.decl.cases.values.toList)
    docDoc(enm.decl.doc)
    docCollapsableSubSection("Instances", sortedInstances, docInstance)
    sb.append("</div>")

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /** Generates the string representing the head of the HTML document. */
  private def mkHead(name: String): String = {
    s"""<!doctype html><html lang='en'>
       |<head>
       |<meta charset='utf-8'>
       |<meta name='viewport' content='width=device-width,initial-scale=1'>
       |<meta name='description' content='API documentation for ${esc(name)}| The Flix Programming Language'>
       |<meta name='keywords' content='Flix, Programming, Language, API, Documentation, ${esc(name)}'>
       |<link href='https://fonts.googleapis.com/css?family=Fira+Code&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Oswald&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Noto+Sans&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Inter&display=swap' rel='stylesheet'>
       |<link href='styles.css' rel='stylesheet'>
       |<link href='favicon.png' rel='icon'>
       |<script defer src='index.js'></script>
       |<title>Flix | ${esc(name)}</title>
       |</head>
    """.stripMargin
  }

  /**
    * Generate the theme toggle button.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docThemeToggle()(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<button id='theme-toggle' disabled aria-label='Toggle theme'>")
    sb.append("<span class='text'>Toggle theme.</span>")
    sb.append("<span class='dark-icon'>")
    inlineIcon("darkMode")
    sb.append("</span>")
    sb.append("<span class='light-icon'>")
    inlineIcon("lightMode")
    sb.append("</span>")
    sb.append("</button>")
  }

  /**
    * Generate the side bar with the contents specified by `docContents`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSideBar(parent: Option[Symbol.ModuleSym])(docContents: () => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<nav>")
    sb.append("<div id='menu-toggle'>")
    sb.append("<input type='checkbox' aria-label='Show/hide sidebar menu'>")
    sb.append("<label for='menu-toggle'>Toggle the menu</label>")
    sb.append("<span class='menu icon'>")
    inlineIcon("menu")
    sb.append("</span>")
    sb.append("<span class='close icon'>")
    inlineIcon("close")
    sb.append("</span>")
    sb.append("</div>")
    sb.append("<div class='sidebar'>")
    sb.append("<div class='flix'>")
    sb.append("<h2><a href='index.html'>flix</a></h2>")
    sb.append(s"<span class='version'>${Version.CurrentVersion}</span>")
    sb.append("</div>")
    parent.map { p =>
      sb.append(s"<a class='back' href='${escUrl(moduleFileName(p))}'>")
      inlineIcon("back")
      sb.append(moduleName(p))
      sb.append("</a>")
    }
    docContents()
    sb.append("</div>")
    sb.append("</nav>")
  }

  /**
    * Documents a section in the side bar, (Modules, Traits, Enums, etc.), containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the section, e.g. "Modules".
    * @param group  The list of items in the section, in the order that they should appear.
    * @param docElt A function taking a single item from `group` and generating the corresponding HTML string.
    *               Note that they will each be wrapped in an `<li>` tag.
    */
  private def docSideBarSection[T](name: String, group: List[T], docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<h3><a href='#${escUrl(name.replace(' ', '-'))}'>${esc(name)}</a></h3>")
    sb.append(s"<ul class='${esc(name.replace(' ', '-'))}'>")
    for (e <- group) {
      sb.append("<li>")
      docElt(e)
      sb.append("</li>")
    }
    sb.append("</ul>")
  }

  private def docSubModules(parentMod: Module)(implicit flix: Flix, sb: StringBuilder): Unit = {
    val subItems: List[Item] =
      parentMod.submodules ++
        parentMod.traits ++
        parentMod.effects ++
        parentMod.enums

    val sortedItems = subItems.sortBy(_.name)

    if (sortedItems.isEmpty) {
      return
    }

    sb.append("<h3>Modules</h3>")
    sb.append("<ul class='Modules'>")
    for (m <- sortedItems) {
      sb.append("<li>")
      sb.append(s"<a href='${escUrl(m.fileName)}'>${esc(m.name)}</a>")
      sb.append("</li>")
    }
    sb.append("</ul>")
  }

  /**
    * Documents a section, (Traits, Enums, Effects, etc.), containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the section, e.g. "Traits".
    *               This name will also be the id of the section.
    * @param group  The list of items in the section, in the order that they should appear.
    * @param docElt A function taking a single item from `group` and generating the corresponding HTML string.
    */
  private def docSection[T](name: String, group: List[T], docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<section id='${name.replace(' ', '-')}'>")
    sb.append(s"<h2>$name</h2>")
    for (e <- group) {
      docElt(e)
    }
    sb.append("</section>")
  }

  /**
    * Documents a subsection, (Signatures, Instances, etc.), containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the subsection, e.g. "Signatures".
    * @param group  The list of items in the section, in the order that they should appear.
    * @param docElt A function taking a single item from `group` and generating the corresponding HTML string.
    */
  private def docSubSection[T](name: String, group: List[T], docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<section class='subsection'>")
    sb.append(s"<h3>${esc(name)}</h3>")
    for (e <- group) {
      docElt(e)
    }
    sb.append("</section>")
  }

  /**
    * Documents a collapsable subsection, containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the subsection, e.g. "Instances".
    * @param group  The list of items in the section, in the order that they should appear.
    * @param docElt A function taking a single item from `group` and generating the corresponding HTML string.
    */
  private def docCollapsableSubSection[T](name: String, group: List[T], docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<details class='subsection'>")
    sb.append(s"<summary><h3>${esc(name)}</h3></summary>")
    for (e <- group) {
      docElt(e)
    }
    sb.append("</details>")
  }

  /**
    * Documents the given `TypeAlias`, `ta`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeAlias(ta: TypedAst.TypeAlias)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='ta-${esc(ta.sym.name)}'>")
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>type alias</span> ")
    sb.append(s"<span class='name'>${esc(ta.sym.name)}</span>")
    docTypeParams(ta.tparams)
    sb.append(" = ")
    docType(ta.tpe)
    sb.append("</code>")
    docActions(Some(s"ta-${esc(ta.sym.name)}"), ta.loc)
    sb.append("</div>")
    docDoc(ta.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given `Def`, `defn`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDef(defn: TypedAst.Def)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='def-${esc(defn.sym.name)}'>")
    docSpec(defn.sym.name, defn.spec, Some(s"def-${esc(defn.sym.name)}"))
    sb.append("</div>")
  }

  /**
    * Documents the given `Sig`, `sig`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSignature(sig: TypedAst.Sig)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='sig-${esc(sig.sym.name)}'>")
    docSpec(sig.sym.name, sig.spec, Some(s"sig-${esc(sig.sym.name)}"))
    sb.append("</div>")
  }

  /**
    * Documents the given `Op`, `op`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docOp(op: TypedAst.Op)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='op-${esc(op.sym.name)}'>")
    docSpec(op.sym.name, op.spec, Some(s"op-${esc(op.sym.name)}"))
    sb.append("</div>")
  }

  /**
    * Documents the given `Spec`, `spec`, with the given `name`.
    * Shared by `Def` and `Sig`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSpec(name: String, spec: TypedAst.Spec, linkId: Option[String])(implicit flix: Flix, sb: StringBuilder): Unit = {
    docAnnotations(spec.ann)
    sb.append("<div class='decl'>")
    sb.append(s"<code>")
    sb.append("<span class='keyword'>def</span> ")
    sb.append(s"<span class='name'>${esc(name)}</span>")
    docFormalParams(spec.fparams)
    sb.append(": ")
    docType(spec.retTpe)
    docEffectType(spec.eff)
    docTraitConstraints(spec.tconstrs)
    docEqualityConstraints(spec.econstrs)
    sb.append("</code>")
    docActions(linkId, spec.loc)
    sb.append("</div>")
    docDoc(spec.doc)
  }

  /**
    * Documents the given associated type of a trait.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docAssoc(assoc: TypedAst.AssocTypeSig)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<div>")
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>type</span> ")
    sb.append(s"<span class='name'>${assoc.sym.name}</span>")
    sb.append(": ")
    docKind(assoc.kind)
    sb.append("</code>")
    docActions(None, assoc.loc)
    sb.append("</div>")
    docDoc(assoc.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given `instance` of a trait.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docInstance(instance: TypedAst.Instance)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<div>")
    docAnnotations(instance.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>instance</span> ")
    docTraitName(instance.trt.sym)
    sb.append("[")
    docType(instance.tpe)
    sb.append("]")
    docTraitConstraints(instance.tconstrs)
    sb.append("</code>")
    docActions(None, instance.loc)
    sb.append("</div>")
    docDoc(instance.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given list of `TraitConstraint`s, `tconsts`.
    * E.g. "with Functor[m]".
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `tconsts` is empty, nothing will be generated.
    */
  private def docTraitConstraints(tconsts: List[Ast.TraitConstraint])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (tconsts.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")
    docList(tconsts.sortBy(_.loc)) { t =>
      docTraitName(t.head.sym)
      sb.append("[")
      docType(t.arg)
      sb.append("]")
    }
    sb.append("</span>")
  }

  /** Document the name of the given trait symbol, creating a link to the trait's documentation. */
  private def docTraitName(sym: Symbol.TraitSym)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<a class='tpe-constraint' href='${escUrl(traitFileName(sym))}' title='trait ${esc(traitName(sym))}'>")
    sb.append(esc(sym.name))
    sb.append("</a>")
  }

  /**
    * Documents the given list of `EqualityConstraint`s, `econsts`.
    * E.g. "where C.T[a] ~ String".
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `econsts` is empty, nothing will be generated.
    */
  private def docEqualityConstraints(econsts: List[Ast.EqualityConstraint])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (econsts.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>where</span> ")
    docList(econsts.sortBy(_.loc)) { e =>
      docTraitName(e.cst.sym.trt)
      sb.append(".")
      sb.append(esc(e.cst.sym.name))
      sb.append("[")
      docType(e.tpe1)
      sb.append("] ~ ")
      docType(e.tpe2)
    }
    sb.append("</span>")
  }

  /**
    * Documents the given `Derivations`s, `derives`.
    * E.g. "with Sendable".
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `derives` contains no elements, nothing will be generated.
    */
  private def docDerivations(derives: Ast.Derivations)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (derives.traits.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")
    docList(derives.traits.sortBy(_.loc)) { t =>
      docTraitName(t.trt)
    }
    sb.append("</span>")
  }

  /**
    * Documents the given list of `Case`s of an enum.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docCases(cases: List[TypedAst.Case])(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<div class='cases'>")
    for (c <- cases.sortBy(_.loc)) {
      sb.append("<code>")
      sb.append("<span class='keyword'>case</span> ")
      sb.append(s"<span class='case-tag'>${esc(c.sym.name)}</span>")

      SimpleType.fromWellKindedType(c.tpe) match {
        case SimpleType.Unit => // Nothing
        case SimpleType.Tuple(elms) =>
          sb.append("(")
          docList(elms) { t =>
            sb.append(s"<span class='type'>${esc(FormatType.formatSimpleType(t))}</span>")
          }
          sb.append(")")
        case _ =>
          sb.append("(")
          docType(c.tpe)
          sb.append(")")
      }

      sb.append("</code>")
    }
    sb.append("</div>")
  }

  /**
    * Documents the given list of `TypeParam`s wrapped in `[]`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeParams(tparams: List[TypedAst.TypeParam])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (tparams.isEmpty) {
      return
    }

    sb.append("<span class='tparams'>[")
    docList(tparams.sortBy(_.loc)) { p =>
      sb.append("<span class='tparam'>")
      sb.append(s"<span class='type'>${esc(p.name.name)}</span>")
      sb.append(": ")
      docKind(p.sym.kind)
      sb.append("</span>")
    }
    sb.append("]</span>")
  }

  /**
    * Document the given list of `FormalParam`s wrapped in `()`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docFormalParams(fparams: List[TypedAst.FormalParam])(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='fparams'>(")
    fparams match {
      case List(TypedAst.FormalParam(_, _, Type.Cst(TypeConstructor.Unit, _), _, _)) =>
      // For a function declared with zero formal parameters,
      // the compiler will introduce a single parameter of the unit type
      case _ =>
        docList(fparams.sortBy(_.loc)) { p =>
          sb.append(s"<span><span>${esc(p.sym.text)}</span>: ")
          docType(p.tpe)
          sb.append("</span>")
        }
    }
    sb.append(")</span>")
  }

  /**
    * Document the given `Annotations`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docAnnotations(anns: Ast.Annotations)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (anns.annotations.isEmpty) {
      return
    }

    sb.append("<code class='annotations'>")
    for (a <- anns.annotations) {
      sb.append(s"<span class='annotation'>${esc(a.toString)}</span> ")
    }
    sb.append("</code>")
  }

  /**
    * Appends a 'copy link' button the the given `StringBuilder`.
    * This creates a link to the given ID on the current URL.
    */
  private def docLink(id: String)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<a href='#${escUrl(id)}' class='copy-link' aria-label='Link'>")
    inlineIcon("link")
    sb.append("</a> ")
  }

  /**
    * Document the given `SourceLocation`, `loc`, in the form of a link.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSourceLocation(loc: SourceLocation)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<a class='source' target='_blank' rel='nofollow' href='${createLink(loc)}'>Source</a>")
  }

  /**
    * Document the right hand actions.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * @param linkId An optional ID in the document, that the 'copy link' button will refer to.
    *               If `None`, the button will not be included.
    * @param loc    The source location that the 'source' button will refer to.
    */
  private def docActions(linkId: Option[String], loc: SourceLocation)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='actions'>")
    linkId.foreach(docLink)
    docSourceLocation(loc)
    sb.append("</span>")
  }

  /**
    * Document the the given `doc`, while parsing any markdown.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDoc(doc: Ast.Doc)(implicit flix: Flix, sb: StringBuilder): Unit = {
    val text = doc.text
    if (text.isBlank) {
      return
    }

    val escaped = esc(text)

    val config =
      txtmark.Configuration.builder()
        .build()
    val parsed = txtmark.Processor.process(escaped, config)

    // Since both esc and process escapes the & character, it needs to be unescaped once
    val unescaped = parsed.replace("&amp;", "&")

    sb.append("<div class='doc'>")
    sb.append(unescaped)
    sb.append("</div>")
  }

  /**
    * Document the the given `Type`, `tpe`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docType(tpe: Type)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='type'>")
    sb.append(esc(FormatType.formatType(tpe)))
    sb.append("</span>")
  }

  /**
    * Document the the given `Kind`, `kind`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docKind(kind: Kind)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='kind'>")
    sb.append(esc(kind.toString))
    sb.append("</span>")
  }

  /**
    * Document the the given `Type`, `eff`, when it is known to be in effect position.
    *
    * For example: `" \ IO"`
    *
    * If this is the pure effect, nothing is written.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffectType(eff: Type)(implicit flix: Flix, sb: StringBuilder): Unit = {
    val simpleEff = SimpleType.fromWellKindedType(eff)
    simpleEff match {
      case SimpleType.Pure => // No op
      case _ =>
        sb.append(" \\ ")
        sb.append("<span class='effect'>")
        sb.append(esc(FormatType.formatSimpleType(simpleEff)))
        sb.append("</span>")
    }
  }

  /** Runs the given `docElt` on each element of `list`, separated by the string: ", " (comma + space) */
  private def docList[T](list: List[T])(docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    for ((e, i) <- list.zipWithIndex) {
      docElt(e)
      if (i < list.length - 1) {
        sb.append(", ")
      }
    }
  }

  /** Make a copy of the static assets into the output directory. */
  private def writeAssets(): Unit = {
    val stylesheet = readResource(Stylesheet)
    writeFile("styles.css", stylesheet)

    val favicon = readResource(FavIcon)
    writeFile("favicon.png", favicon)

    val script = readResource(Script)
    writeFile("index.js", script)
  }

  /**
    * Append the contents of the SVG file with the given `name` to the given `StringBuilder`.
    *
    * By inlining the icon into the HTML itself, it can inherit the `color` of its parent.
    */
  private def inlineIcon(name: String)(implicit sb: StringBuilder): Unit = {
    sb.append(readResourceString(s"$Icons/$name.svg"))
  }

  /** Write the documentation output string into the output directory with the given `name`. */
  private def writeDocFile(name: String, output: String): Unit = {
    writeFile(s"$name", output.getBytes)
  }

  /** Write the file to the output directory with the given file name. */
  private def writeFile(name: String, output: Array[Byte]): Unit = {
    val path = OutputDirectory.resolve(name)
    try {
      Files.createDirectories(OutputDirectory)
      Files.write(path, output)
    } catch {
      case ex: IOException => throw new RuntimeException(s"Unable to write to path '$path'.", ex)
    }
  }

  /**
    * Reads the given resource as an array of bytes.
    *
    * @param path The path of the resource, relative to the resources folder.
    */
  private def readResource(path: String): Array[Byte] = {
    val is = LocalResource.getInputStream(path)
    LazyList.continually(is.read).takeWhile(_ != -1).map(_.toByte).toArray
  }

  /**
    * Reads the given resource as a string.
    *
    * @param path The path of the resource, relative to the resources folder.
    */
  private def readResourceString(path: String): String = LocalResource.get(path)

  /**
    * Create a raw link to the given `SourceLocation`.
    *
    * The URL is already escaped.
    */
  private def createLink(loc: SourceLocation): String = {
    // TODO make it also work for local user code
    s"$LibraryGitHub${escUrl(loc.source.name)}#L${loc.beginLine}-L${loc.endLine}"
  }

  /** Escape any HTML in the string. */
  private def esc(s: String): String = xml.Utility.escape(s)

  /** Transform the string into a valid URL. */
  private def escUrl(s: String): String = URLEncoder.encode(s, "UTF-8")

  /** An item is a unit that is typically output to its own HTML file. */
  private sealed trait Item {
    /** The shortest name of the item, e.g. 'StdOut' */
    def name: String

    /** The fully qualified name of the item, e.g. 'System.StdOut' */
    def qualifiedName: String

    /** The file name of the item, e.g. 'System.StdOut.html' */
    def fileName: String
  }

  /** A representation of a module that's easier to work with while generating documentation. */
  private case class Module(sym: Symbol.ModuleSym,
                            parent: Option[Symbol.ModuleSym],
                            uses: List[Ast.UseOrImport],
                            submodules: List[Module],
                            traits: List[Trait],
                            effects: List[Effect],
                            enums: List[Enum],
                            typeAliases: List[TypedAst.TypeAlias],
                            defs: List[TypedAst.Def]) extends Item {
    override def name: String = moduleName(this.sym)

    override def qualifiedName: String = moduleQualifiedName(this.sym)

    override def fileName: String = moduleFileName(this.sym)
  }

  /** A representation of a trait that's easier to work with while generating documentation. */
  private case class Trait(decl: TypedAst.Trait,
                           signatures: List[TypedAst.Sig],
                           defs: List[TypedAst.Sig],
                           instances: List[TypedAst.Instance],
                           parent: Symbol.ModuleSym,
                           companionMod: Option[Module]) extends Item {
    override def name: String = traitName(this.decl.sym)

    override def qualifiedName: String = traitQualifiedName(this.decl.sym)

    override def fileName: String = traitFileName(this.decl.sym)
  }

  /** A representation of an effect that's easier to work with while generating documentation. */
  private case class Effect(decl: TypedAst.Effect,
                            parent: Symbol.ModuleSym,
                            companionMod: Option[Module]) extends Item {
    override def name: String = effectName(this.decl.sym)

    override def qualifiedName: String = effectQualifiedName(this.decl.sym)

    override def fileName: String = effectFileName(this.decl.sym)
  }

  /** A representation of an enum that's easier to work with while generating documentation. */
  private case class Enum(decl: TypedAst.Enum,
                          instances: List[TypedAst.Instance],
                          parent: Symbol.ModuleSym,
                          companionMod: Option[Module]) extends Item {
    override def name: String = enumName(this.decl.sym)

    override def qualifiedName: String = enumQualifiedName(this.decl.sym)

    override def fileName: String = enumFileName(this.decl.sym)
  }
}
