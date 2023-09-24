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
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatType, SimpleType}
import ca.uwaterloo.flix.util.LocalResource

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import com.github.rjeschke.txtmark

import java.net.URLEncoder

/**
  * A phase that emits a JSON file for library documentation.
  */
object HtmlDocumentor {

  /**
    * The "Pseudo-name" of the root namespace displayed on the pages.
    */
  val RootNS: String = "Prelude"
  /**
    * The "Pseudo-name" of the root namespace used for its file name.
    */
  val RootFileName: String = "index"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./build/doc")

  /**
    * The path to the the stylesheet, relative to the resources folder.
    */
  val Stylesheet: String = "/doc/styles.css"

  /**
    * The path to the the favicon, relative to the resources folder.
    */
  val FavIcon: String = "/doc/favicon.png"

  /**
    * The path to the the script, relative to the resources folder.
    */
  val Script: String = "/doc/index.js"

  /**
    * The path to the the icon directory, relative to the resources folder.
    */
  val Icons: String = "/doc/icons"

  /**
    * The root of the link to each file of the standard library.
    */
  val LibraryGitHub: String = "https://github.com/flix/flix/blob/master/main/src/library/"

  def run(root: TypedAst.Root)(implicit flix: Flix): Unit = {
    val modulesRoot = splitModules(root)
    val filteredModulesRoot = filterModules(modulesRoot)

    def visitMod(mod: Module): Unit = {
      val out = documentModule(mod)
      writeDocFile(moduleFileName(mod.sym), out)

      mod.submodules.foreach(visitMod)
      mod.classes.foreach(visitClass)
      mod.effects.foreach(visitEffect)
      mod.enums.foreach(visitEnum)
    }

    def visitClass(clazz: Class): Unit = {
      val out = documentClass(clazz)
      writeDocFile(classFileName(clazz.decl.sym), out)

      clazz.companionMod.foreach {
        _.submodules.foreach(visitMod)
      }
    }

    def visitEffect(eff: Effect): Unit = {
      val out = documentEffect(eff)
      writeDocFile(effectFileName(eff.decl.sym), out)

      eff.companionMod.foreach {
        _.submodules.foreach(visitMod)
      }
    }

    def visitEnum(enm: Enum): Unit = {
      val out = documentEnum(enm)
      writeDocFile(enumFileName(enm.decl.sym), out)

      enm.companionMod.foreach {
        _.submodules.foreach(visitMod)
      }
    }

    visitMod(filteredModulesRoot)
    writeAssets()
  }

  /**
    * Get the display name of the module.
    *
    * See also `moduleFileName` for the file name of the module.
    */
  private def moduleName(sym: Symbol.ModuleSym): String = if (sym.isRoot) RootNS else sym.toString

  /**
    * Get the file name of the module.
    */
  private def moduleFileName(sym: Symbol.ModuleSym): String = s"${if (sym.isRoot) RootFileName else sym.toString}.html"

  /**
    * Get the display name of the class.
    *
    * See also `classFileName` for the file name of the class.
    */
  private def className(sym: Symbol.ClassSym): String = sym.toString

  /**
    * Get the file name of the class.
    */
  private def classFileName(sym: Symbol.ClassSym): String = s"${sym.toString}.html"

  /**
    * Get the display name of the effect.
    *
    * See also `effectFileName` for the file name of the effect.
    */
  private def effectName(sym: Symbol.EffectSym): String = sym.toString

  /**
    * Get the file name of the effect.
    */
  private def effectFileName(sym: Symbol.EffectSym): String = s"${sym.toString}.html"

  /**
    * Get the display name of the enum.
    *
    * See also `enumFileName` for the file name of the enum.
    */
  private def enumName(sym: Symbol.EnumSym): String = sym.toString

  /**
    * Get the file name of the enum.
    */
  private def enumFileName(sym: Symbol.EnumSym): String = s"${sym.toString}.html"

  /**
    * Splits the modules present in the root into a tree of `HtmlDocumentor.Module`s, making them easier to work with.
    */
  private def splitModules(root: TypedAst.Root): Module = {

    /**
      * Visits a module and all of its submodules
      */
    def visitMod(moduleSym: Symbol.ModuleSym, parent: Option[Symbol.ModuleSym]): Module = {
      val mod = root.modules(moduleSym)
      val uses = root.uses.getOrElse(moduleSym, Nil)

      /** Modules that should not be included as a submodule */
      var companionMods: List[Symbol.ModuleSym] = Nil

      var submodules: List[Symbol.ModuleSym] = Nil
      var classes: List[Class] = Nil
      var effects: List[Effect] = Nil
      var enums: List[Enum] = Nil
      var typeAliases: List[TypedAst.TypeAlias] = Nil
      var defs: List[TypedAst.Def] = Nil
      mod.foreach {
        case sym: Symbol.ModuleSym => submodules = sym :: submodules
        case sym: Symbol.ClassSym =>
          val companionMod = companionModule(sym.namespace :+ sym.name, moduleSym, root)
          companionMod.foreach(m => companionMods = m.sym :: companionMods)
          classes = mkClass(sym, moduleSym, companionMod, root) :: classes
        case sym: Symbol.EffectSym =>
          val companionMod = companionModule(sym.namespace :+ sym.name, moduleSym, root)
          companionMod.foreach(m => companionMods = m.sym :: companionMods)
          effects = mkEffect(sym, moduleSym, companionMod, root) :: effects
        case sym: Symbol.EnumSym =>
          val companionMod = companionModule(sym.namespace :+ sym.name, moduleSym, root)
          companionMod.foreach(m => companionMods = m.sym :: companionMods)
          enums = mkEnum(sym, moduleSym, companionMod, root) :: enums
        case sym: Symbol.TypeAliasSym => typeAliases = root.typeAliases(sym) :: typeAliases
        case sym: Symbol.DefnSym => defs = root.defs(sym) :: defs
        case _ => // No op
      }

      submodules = submodules.filterNot(companionMods.contains)

      Module(
        moduleSym,
        parent,
        uses,
        submodules.map(visitMod(_, Some(moduleSym))),
        classes,
        effects,
        enums,
        typeAliases,
        defs,
      )
    }

    /**
      * Get the optional companion module for the item with the given `namespace`.
      * `namespace` should include the name of the item itself.
      */
    def companionModule(namespace: List[String], parent: Symbol.ModuleSym, root: TypedAst.Root): Option[Module] = {
      val sym = Symbol.mkModuleSym(namespace)
      if (root.modules.contains(sym)) Some(visitMod(sym, Some(parent)))
      else None
    }

    visitMod(Symbol.mkModuleSym(Nil), None)
  }

  /**
    * Extracts all relevant information about the given `ClassSym` from the root, into a `HtmlDocumentor.Class`.
    */
  private def mkClass(sym: Symbol.ClassSym, parent: Symbol.ModuleSym, companionMod: Option[Module], root: TypedAst.Root): Class = {
    val decl = root.classes(sym)

    val (sigs, defs) = decl.signatures.partition(_.exp.isEmpty)
    val instances = root.instances.getOrElse(sym, Nil)

    Class(decl, sigs, defs, instances, parent, companionMod)
  }

  /**
    * Extracts all relevant information about the given `EffectSym` from the root, into a `HtmlDocumentor.Effect`.
    */
  private def mkEffect(sym: Symbol.EffectSym, parent: Symbol.ModuleSym, companionMod: Option[HtmlDocumentor.Module], root: TypedAst.Root): Effect = {
    Effect(root.effects(sym), parent, companionMod)
  }

  /**
    * Extracts all relevant information about the given `EnumSym` from the root, into a `HtmlDocumentor.Enum`.
    */
  private def mkEnum(sym: Symbol.EnumSym, parent: Symbol.ModuleSym, companionMod: Option[HtmlDocumentor.Module], root: TypedAst.Root): Enum = {
    Enum(root.enums(sym), parent, companionMod)
  }

  /**
    * Filter the module, `mod`, and its children, removing all items and empty modules, which shouldn't appear in the documentation.
    */
  private def filterModules(mod: Module): Module = {
    filterEmpty(filterItems(mod))
  }

  /**
    * Returns a tree of modules corresponding to the given input,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterItems(mod: Module): Module = mod match {
    case Module(sym, parent, uses, submodules, classes, effects, enums, typeAliases, defs) =>
      Module(
        sym,
        parent,
        uses,
        submodules.map(filterItems),
        classes.filter(c => c.decl.mod.isPublic && !c.decl.ann.isInternal).map(filterClass),
        effects.filter(e => e.decl.mod.isPublic && !e.decl.ann.isInternal).map(filterEffect),
        enums.filter(e => e.decl.mod.isPublic && !e.decl.ann.isInternal).map(filterEnum),
        typeAliases.filter(t => t.mod.isPublic),
        defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
      )
  }

  /**
    * Returns a `Class` corresponding to the given `clazz`,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterClass(clazz: Class): Class = clazz match {
    case Class(TypedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, _, laws, loc), signatures, defs, instances, parent, companionMod) =>
      Class(
        TypedAst.Class(
          doc,
          ann,
          mod,
          sym,
          tparam,
          superClasses,
          assocs.filter(a => a.mod.isPublic),
          Nil,
          laws.filter(l => l.spec.mod.isPublic && !l.spec.ann.isInternal),
          loc
        ),
        signatures.filter(s => s.spec.mod.isPublic && !s.spec.ann.isInternal),
        defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
        instances.filter(i => i.mod.isPublic && !i.ann.isInternal),
        parent,
        companionMod.map(filterItems)
      )
  }

  /**
    * Returns an `Effect` corresponding to the given `eff`,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterEffect(eff: Effect): Effect = eff match {
    case Effect(eff, parent, companionMod) =>
      Effect(eff, parent, companionMod.map(filterItems))
  }

  /**
    * Returns an `Enum` corresponding to the given `enm`,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterEnum(enm: Enum): Enum = enm match {
    case Enum(enm, parent, companionMod) =>
      Enum(enm, parent, companionMod.map(filterItems))
  }

  /**
    * Remove any modules and references to them if they:
    *   1. Contain no items
    *   1. Contain no submodules with any items
    */
  private def filterEmpty(mod: Module): Module = {
    /**
      * Recursively walks the module tree removing empty modules.
      * These modules are removed from the map, and from the `submodules` field.
      *
      * Returns a boolean, describing whether or not this module is included.
      */
    def visitMod(mod: Module): Option[Module] = {
      mod match {
        case Module(sym, parent, uses, submodules, classes, effects, enums, typeAliases, defs) =>
          val filteredSubMods = submodules.flatMap(visitMod)
          val filteredClasses = classes.map {
            case Class(decl, signatures, defs, instances, parent, companionMod) =>
              Class(decl, signatures, defs, instances, parent, companionMod.flatMap(visitMod))
          }
          val filteredEffects = effects.map {
            case Effect(decl, parent, companionMod) =>
              Effect(decl, parent, companionMod.flatMap(visitMod))
          }
          val filteredEnums = enums.map {
            case Enum(decl, parent, companionMod) =>
              Enum(decl, parent, companionMod.flatMap(visitMod))
          }

          val isEmpty =
            filteredSubMods.isEmpty &&
              filteredClasses.isEmpty &&
              filteredEffects.isEmpty &&
              filteredEnums.isEmpty &&
              typeAliases.isEmpty &&
              defs.isEmpty

          if (isEmpty) None
          else Some(
            Module(
              sym,
              parent,
              uses,
              filteredSubMods,
              filteredClasses,
              filteredEffects,
              filteredEnums,
              typeAliases,
              defs
            )
          )
      }
    }

    visitMod(mod).get
  }

  /**
    * Documents the given `Module`, `mod`, returning a string of HTML.
    */
  private def documentModule(mod: Module)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedMods = mod.submodules.sortBy(_.sym.ns.last)
    val sortedClasses = mod.classes.sortBy(_.decl.sym.name)
    val sortedEnums = mod.enums.sortBy(_.decl.sym.name)
    val sortedEffs = mod.effects.sortBy(_.decl.sym.name)
    val sortedTypeAliases = mod.typeAliases.sortBy(_.sym.name)
    val sortedDefs = mod.defs.sortBy(_.sym.name)

    sb.append(mkHead(moduleName(mod.sym)))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar { () =>
      mod.parent.map {
        mod => sb.append(s"<a class='back' href='${esc(moduleFileName(mod))}'>${moduleName(mod)}</a>")
      }
      docSubModules(sortedMods)
      docSideBarSection(
        "Classes",
        sortedClasses,
        (c: Class) => sb.append(s"<a href='${esc(classFileName(c.decl.sym))}'>${esc(c.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(effectFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${esc(enumFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
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
    sb.append(s"<h1>${esc(moduleName(mod.sym))}</h1>")
    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedDefs, docDef)
    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /**
    * Documents the given `Class`, `clazz`, returning a string of HTML.
    */
  private def documentClass(clazz: Class)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedSigs = clazz.signatures.sortBy(_.sym.name)
    val sortedClassDefs = clazz.defs.sortBy(_.sym.name)

    val mod = clazz.companionMod
    val sortedMods = mod.map(_.submodules).getOrElse(Nil).sortBy(_.sym.ns.last)
    val sortedClasses = mod.map(_.classes).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(className(clazz.decl.sym)))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar { () =>
      sb.append(s"<a class='back' href='${esc(moduleFileName(clazz.parent))}'>${moduleName(clazz.parent)}</a>")
      docSubModules(sortedMods)
      docSideBarSection(
        "Signatures",
        sortedSigs,
        (s: TypedAst.Sig) => sb.append(s"<a href='#sig-${escUrl(s.sym.name)}'>${esc(s.sym.name)}</a>"),
      )
      docSideBarSection(
        "Class Definitions",
        sortedClassDefs,
        (d: TypedAst.Sig) => sb.append(s"<a href='#sig-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
      docSideBarSection(
        "Classes",
        sortedClasses,
        (c: Class) => sb.append(s"<a href='${esc(classFileName(c.decl.sym))}'>${esc(c.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(effectFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${esc(enumFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
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
    sb.append(s"<h1>${esc(className(clazz.decl.sym))}</h1>")

    sb.append("<section>")
    sb.append(s"<div class='box'>")
    docAnnotations(clazz.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>class</span> ")
    sb.append(s"<span class='name'>${esc(clazz.decl.sym.name)}</span>")
    docTypeParams(List(clazz.decl.tparam))
    docTypeConstraints(clazz.decl.superClasses)
    sb.append("</code>")
    docActions(None, clazz.decl.loc)
    sb.append("</div>")
    docDoc(clazz.decl.doc)
    docSubSection("Instances", clazz.instances.sortBy(_.loc), docInstance)
    sb.append("</div>")
    sb.append("</section>")

    docSection("Signatures", sortedSigs, docSignature)
    docSection("Class Definitions", sortedClassDefs, docSignature)

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Module Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /**
    * Documents the given `Effect`, `eff`, returning a string of HTML.
    */
  private def documentEffect(eff: Effect)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedOps = eff.decl.ops.sortBy(_.sym.name)

    val mod = eff.companionMod
    val sortedMods = mod.map(_.submodules).getOrElse(Nil).sortBy(_.sym.ns.last)
    val sortedClasses = mod.map(_.classes).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(effectName(eff.decl.sym)))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar { () =>
      sb.append(s"<a class='back' href='${escUrl(moduleFileName(eff.parent))}'>${moduleName(eff.parent)}</a>")
      docSubModules(sortedMods)
      docSideBarSection(
        "Operations",
        sortedOps, (o: TypedAst.Op) => sb.append(s"<a href='#op-${escUrl(esc(o.sym.name))}'>${esc(o.sym.name)}</a>")
      )
      docSideBarSection(
        "Classes",
        sortedClasses,
        (c: Class) => sb.append(s"<a href='${escUrl(classFileName(c.decl.sym))}'>${esc(c.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(effectFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(enumFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
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
    sb.append(s"<h1>${esc(effectName(eff.decl.sym))}</h1>")

    sb.append("<section>")
    sb.append(s"<div class='box' id='eff-${esc(effectName(eff.decl.sym))}'>")
    docAnnotations(eff.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>eff</span> ")
    sb.append(s"<span class='name'>${esc(eff.decl.sym.name)}</span>")
    sb.append("</code>")
    docActions(None, eff.decl.loc)
    sb.append("</div>")
    docDoc(eff.decl.doc)
    sb.append("</div>")
    sb.append("</section>")

    docSection("Operations", sortedOps, docOp)

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /**
    * Documents the given `Enum`, `enm`, returning a string of HTML.
    */
  private def documentEnum(enm: Enum)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val mod = enm.companionMod
    val sortedMods = mod.map(_.submodules).getOrElse(Nil).sortBy(_.sym.ns.last)
    val sortedClasses = mod.map(_.classes).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.decl.sym.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(enumName(enm.decl.sym)))
    sb.append("<body class='no-script'>")

    docThemeToggle()

    docSideBar { () =>
      sb.append(s"<a class='back' href='${escUrl(moduleFileName(enm.parent))}'>${moduleName(enm.parent)}</a>")
      docSubModules(sortedMods)
      docSideBarSection(
        "Classes",
        sortedClasses,
        (c: Class) => sb.append(s"<a href='${escUrl(classFileName(c.decl.sym))}'>${esc(c.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(effectFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(enumFileName(e.decl.sym))}'>${esc(e.decl.sym.name)}</a>"),
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
    sb.append(s"<h1>${esc(enumName(enm.decl.sym))}</h1>")

    sb.append("<section>")
    sb.append(s"<div class='box' id='enum-${esc(enm.decl.sym.name)}'>")
    docAnnotations(enm.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>enum</span> ")
    sb.append(s"<span class='name'>${esc(enm.decl.sym.name)}</span>")
    docTypeParams(enm.decl.tparams)
    docDerivations(enm.decl.derives)
    sb.append("</code>")
    docActions(None, enm.decl.loc)
    sb.append("</div>")
    docCases(enm.decl.cases.values.toList)
    docDoc(enm.decl.doc)
    sb.append("</div>")
    sb.append("</section>")

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /**
    * Generates the string representing the head of the HTML document.
    */
  private def mkHead(name: String): String = {
    s"""<!doctype html><html lang='en'>
      |<head>
      |<meta charset='utf-8'>
      |<meta name='viewport' content='width=device-width,initial-scale=1'>
      |<meta name='description' content='API documentation for ${esc(name)} | The Flix Programming Language'>
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
    sb.append("<button id='theme-toggle' disabled aria-label='Toggle theme' aria-describedby='no-script'>")
    sb.append("<span>Toggle theme.</span>")
    sb.append("<span role='tooltip' id='no-script'>Requires JavaScript</span>")
    sb.append("</button>")
  }

  /**
    * Generate the side bar with the contents specified by `docContents`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSideBar(docContents: () => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<nav>")
    sb.append("<input type='checkbox' id='menu-toggle' aria-label='Show/hide sidebar menu'>")
    sb.append("<label for='menu-toggle'>Toggle the menu</label>")
    sb.append("<div>")
    sb.append("<div class='flix'>")
    sb.append("<h2><a href='index.html'>flix</a></h2>")
    sb.append(s"<span class='version'>${Version.CurrentVersion}</span>")
    sb.append("</div>")
    docContents()
    sb.append("</div>")
    sb.append("</nav>")
  }

  /**
    * Documents a section in the side bar, (Modules, Classes, Enums, etc.), containing a `group` of items.
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

    sb.append(s"<h3><a href='#${name.replace(' ', '-')}'>$name</a></h3>")
    sb.append(s"<ul class='${name.replace(' ', '-')}'>")
    for (e <- group) {
      sb.append("<li>")
      docElt(e)
      sb.append("</li>")
    }
    sb.append("</ul>")
  }

  private def docSubModules(submodules: List[Module])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (submodules.isEmpty) {
      return
    }

    sb.append("<h3>Modules</h3>")
    sb.append("<ul class='Modules'>")
    for (m <- submodules) {
      sb.append("<li>")
      sb.append(s"<a href='${esc(moduleFileName(m.sym))}'>${esc(m.sym.ns.last)}</a>")
      sb.append("</li>")
    }
    sb.append("</ul>")
  }

  /**
    * Documents a section, (Classes, Enums, Effects, etc.), containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the section, e.g. "Classes".
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
    * Documents a collapsable subsection, (Signatures, Instances, etc.), containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the subsection, e.g. "Signatures".
    * @param group  The list of items in the section, in the order that they should appear.
    * @param docElt A function taking a single item from `group` and generating the corresponding HTML string.
    * @param open   Whether or not the subsection is opened by default. Default to false.
    */
  private def docSubSection[T](name: String, group: List[T], docElt: T => Unit, open: Boolean = false)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<details class='subsection' ${if (open) "open" else ""}>")
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
    docTypeConstraints(spec.tconstrs)
    sb.append("</code>")
    docActions(linkId, spec.loc)
    sb.append("</div>")
    docDoc(spec.doc)
  }

  /**
    * Documents the given `instance` of a class.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docInstance(instance: TypedAst.Instance)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<div>")
    docAnnotations(instance.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>instance</span> ")
    docType(instance.tpe)
    docTypeConstraints(instance.tconstrs)
    sb.append("</code>")
    docActions(None, instance.loc)
    sb.append("</div>")
    docDoc(instance.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given list of `TypeConstraint`s, `tconsts`.
    * E.g. "with Functor[m]".
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `tconsts` is empty, nothing will be generated.
    */
  private def docTypeConstraints(tconsts: List[Ast.TypeConstraint])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (tconsts.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")
    docList(tconsts.sortBy(_.loc)) { t =>
      sb.append(s"<span class='tpe-constraint'>${esc(t.head.sym.name)}</span>[")
      docType(t.arg)
      sb.append("]")
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
    if (derives.classes.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")
    docList(derives.classes.sortBy(_.loc)) { c =>
      sb.append(s"<span class='tpe-constraint'>${esc(c.clazz.name)}</span>")
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
      sb.append(s"<span class='case-tag'>${esc(c.sym.name)}</span>(")

      SimpleType.fromWellKindedType(c.tpe)(flix.getFormatOptions) match {
        case SimpleType.Tuple(elms) =>
          docList(elms) { t =>
            sb.append(s"<span class='type'>${esc(FormatType.formatSimpleType(t))}</span>")
          }
        case _ => docType(c.tpe)
      }

      sb.append(")</code>")
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
      sb.append(s": <span class='kind'>${esc(p.sym.kind.toString)}</span>")
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
    docList(fparams.sortBy(_.loc)) { p =>
      sb.append(s"<span><span>${esc(p.sym.text)}</span>: ")
      docType(p.tpe)
      sb.append("</span>")
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
    sb.append(s"<a href='#$id' class='copy-link' aria-label='Link'>")
    inlineIcon("link")
    sb.append("</a> ")
  }

  /**
    * Document the given `SourceLocation`, `loc`, in the form of a link.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSourceLocation(loc: SourceLocation)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<a class='source' target='_blank' href='${esc(createLink(loc))}'>Source</a>")
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
    // Panic mode will escape all < and > characters
    val config =
      txtmark.Configuration.builder()
      .enableSafeMode()
      .enablePanicMode()
      .build()
    val parsed = txtmark.Processor.process(doc.text, config)

    sb.append("<div class='doc'>")
    sb.append(parsed)
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
    * Document the the given `Type`, `eff`, when it is known to be in effect position.
    *
    * For example: `" \ IO"`
    *
    * If this is the pure effect, nothing is written.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffectType(eff: Type)(implicit flix: Flix, sb: StringBuilder): Unit = {
    val simpleEff = SimpleType.fromWellKindedType(eff)(flix.getFormatOptions)
    simpleEff match {
      case SimpleType.Empty => // No op
      case _ =>
        sb.append(" \\ ")
        sb.append("<span class='effect'>")
        sb.append(esc(FormatType.formatSimpleType(simpleEff)))
        sb.append("</span>")
    }
  }

  /**
    * Runs the given `docElt` on each element of `list`, separated by the string: ", " (comma + space)
    */
  private def docList[T](list: List[T])(docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    for ((e, i) <- list.zipWithIndex) {
      docElt(e)
      if (i < list.length - 1) {
        sb.append(", ")
      }
    }
  }

  /**
    * Make a copy of the static assets into the output directory.
    */
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

  /**
    * Write the documentation output string into the output directory with the given `name`.
    */
  private def writeDocFile(name: String, output: String): Unit = {
    writeFile(s"$name", output.getBytes)
  }

  /**
    * Write the file to the output directory with the given file name.
    */
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
    */
  private def createLink(loc: SourceLocation): String = {
    // TODO make it also work for local user code
    s"$LibraryGitHub${loc.source.name}#L${loc.beginLine}-L${loc.beginLine}"
  }

  /**
    * Escape any HTML in the string.
    */
  private def esc(s: String): String = xml.Utility.escape(s)

  /**
    * Transform the string into a valid URL.
    */
  private def escUrl(s: String): String = URLEncoder.encode(s, "UTF-8")

  /**
    * A represention of a module that's easier to work with while generating documention.
    */
  private case class Module(sym: Symbol.ModuleSym,
                            parent: Option[Symbol.ModuleSym],
                            uses: List[Ast.UseOrImport],
                            submodules: List[Module],
                            classes: List[Class],
                            effects: List[Effect],
                            enums: List[Enum],
                            typeAliases: List[TypedAst.TypeAlias],
                            defs: List[TypedAst.Def])

  /**
    * A represention of a class that's easier to work with while generating documention.
    */
  private case class Class(decl: TypedAst.Class,
                           signatures: List[TypedAst.Sig],
                           defs: List[TypedAst.Sig],
                           instances: List[TypedAst.Instance],
                           parent: Symbol.ModuleSym,
                           companionMod: Option[Module])

  /**
    * A represention of an effect that's easier to work with while generating documention.
    */
  private case class Effect(decl: TypedAst.Effect,
                            parent: Symbol.ModuleSym,
                            companionMod: Option[Module])

  /**
    * A represention of an enum that's easier to work with while generating documention.
    */
  private case class Enum(decl: TypedAst.Enum,
                          parent: Symbol.ModuleSym,
                          companionMod: Option[Module])
}
