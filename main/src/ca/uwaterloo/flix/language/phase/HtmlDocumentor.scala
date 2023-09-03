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
import scala.collection.mutable

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
    val modules = splitModules(root)
    val filteredModules = filterModules(modules)
    filteredModules.foreach {
      case (sym, mod) =>
        val out = documentModule(mod)
        writeModule(mod, out)
    }
    writeAssets()
  }

  /**
    * Get the display name of the module.
    *
    * See also `moduleFileName` for the file name of the module.
    */
  private def moduleName(mod: Module): String = if (mod.sym.isRoot) RootNS else mod.sym.toString

  /**
    * Get the file name of the module.
    */
  private def moduleFileName(mod: Module): String = if (mod.sym.isRoot) RootFileName else mod.sym.toString

  /**
    * Splits the modules present in the root into a set of `HtmlDocumentor.Module`s, making them easier to work with.
    */
  private def splitModules(root: TypedAst.Root): Map[Symbol.ModuleSym, Module] = root.modules.map {
    case (sym, mod) =>
      val uses = root.uses.getOrElse(sym, Nil)

      var submodules: List[Symbol.ModuleSym] = Nil
      var classes: List[Class] = Nil
      var enums: List[TypedAst.Enum] = Nil
      var effects: List[TypedAst.Effect] = Nil
      var typeAliases: List[TypedAst.TypeAlias] = Nil
      var defs: List[TypedAst.Def] = Nil
      mod.foreach {
        case sym: Symbol.ModuleSym => submodules = sym :: submodules
        case sym: Symbol.ClassSym => classes = mkClass(sym, root) :: classes
        case sym: Symbol.EnumSym => enums = root.enums(sym) :: enums
        case sym: Symbol.EffectSym => effects = root.effects(sym) :: effects
        case sym: Symbol.TypeAliasSym => typeAliases = root.typeAliases(sym) :: typeAliases
        case sym: Symbol.DefnSym => defs = root.defs(sym) :: defs
        case _ => // No op
      }

      sym -> Module(
        sym,
        uses,
        submodules,
        classes,
        enums,
        effects,
        typeAliases,
        defs,
      )
  }

  /**
    * Extracts all relevant information about the given `ClassSym` from the root, into a `HtmlDocumentor.Class`.
    */
  private def mkClass(sym: Symbol.ClassSym, root: TypedAst.Root): Class = root.classes(sym) match {
    case TypedAst.Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs0, laws, loc) =>

      val (sigs, defs) = sigs0.partition(_.impl.isEmpty)
      val instances = root.instances.getOrElse(sym, Nil)

      Class(doc, ann, mod, sym, tparam, superClasses, assocs, sigs, defs, laws, instances, loc)
  }

  /**
    * Filter the map of modules, removing all items and empty modules, which shouldn't appear in the documentation.
    */
  private def filterModules(mods: Map[Symbol.ModuleSym, HtmlDocumentor.Module]): Map[Symbol.ModuleSym, Module] = {
    filterEmpty(filterItems(mods))
  }

  /**
    * Returns a map of modules corresponding to the given input,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterItems(mods: Map[Symbol.ModuleSym, Module]): Map[Symbol.ModuleSym, Module] = mods.map {
    case (sym, Module(_, uses, submodules, classes, enums, effects, typeAliases, defs)) =>
      sym -> Module(
        sym,
        uses,
        submodules,
        classes.filter(c => c.mod.isPublic && !c.ann.isInternal).map(filterClass),
        enums.filter(e => e.mod.isPublic && !e.ann.isInternal),
        effects.filter(e => e.mod.isPublic && !e.ann.isInternal),
        typeAliases.filter(t => t.mod.isPublic),
        defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
      )
  }


  /**
    * Returns a `Class` corresponding to the given `clazz`,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterClass(clazz: Class): Class = clazz match {
    case Class(doc, ann, mod, sym, tparam, superClasses, assoc, signatures, defs, laws, instances, loc) =>
      Class(
        doc,
        ann,
        mod,
        sym,
        tparam,
        superClasses,
        assoc.filter(a => a.mod.isPublic),
        signatures.filter(s => s.spec.mod.isPublic && !s.spec.ann.isInternal),
        defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
        laws.filter(l => l.spec.mod.isPublic && !l.spec.ann.isInternal),
        instances.filter(i => !i.ann.isInternal),
        loc,
      )
  }

  /**
    * Remove any modules and references to them if they:
    *   1. Contain no public items
    *   1. Contain no submodules with any public items
    */
  private def filterEmpty(mods: Map[Symbol.ModuleSym, Module]): Map[Symbol.ModuleSym, Module] = {
    val modMap: mutable.Map[Symbol.ModuleSym, Module] = mutable.Map(mods.toSeq: _*)

    /**
      * Recursively walks the module tree removing empty modules.
      * These modules are removed from the map, and from the `submodules` field.
      *
      * Returns a boolean, describing whether or not this module is included.
      */
    def checkMod(sym: Symbol.ModuleSym): Boolean = {
      modMap(sym) match {
        case Module(_, uses, submodules, classes, enums, effects, typeAliases, defs) =>
          val filteredSubMods = submodules.filter(checkMod)

          val isEmpty = filteredSubMods.isEmpty &&
            classes.isEmpty &&
            enums.isEmpty &&
            effects.isEmpty &&
            typeAliases.isEmpty &&
            defs.isEmpty

          if (isEmpty) modMap.remove(sym)
          else modMap += sym -> Module(sym, uses, filteredSubMods, classes, enums, effects, typeAliases, defs)

          !isEmpty
      }
    }

    val root = Symbol.mkModuleSym(Nil)
    checkMod(root)
    Map(modMap.toSeq: _*)
  }

  /**
    * Documents the given `Module`, `mod`, returning a string of HTML.
    */
  private def documentModule(mod: Module)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedMods = mod.submodules.sortBy(_.ns.last)
    val sortedClasses = mod.classes.sortBy(_.sym.name)
    val sortedEnums = mod.enums.sortBy(_.sym.name)
    val sortedEffs = mod.effects.sortBy(_.sym.name)
    val sortedTypeAliases = mod.typeAliases.sortBy(_.sym.name)
    val sortedDefs = mod.defs.sortBy(_.sym.name)

    sb.append(mkHead(moduleName(mod)))
    sb.append("<body class='no-script'>")

    sb.append("<button id='theme-toggle' disabled aria-label='Toggle theme' aria-describedby='no-script'>")
    sb.append("<span>Toggle theme.</span>")
    sb.append("<span role='tooltip' id='no-script'>Requires JavaScript</span>")
    sb.append("</button>")

    sb.append("<nav>")
    sb.append("<input type='checkbox' id='menu-toggle' aria-label='Show/hide sidebar menu'>")
    sb.append("<label for='menu-toggle'>Toggle the menu</label>")
    sb.append("<div>")
    sb.append("<div class='flix'>")
    sb.append("<h2><a href='index.html'>flix</a></h2>")
    sb.append(s"<span class='version'>${Version.CurrentVersion}</span>")
    sb.append("</div>")
    docSubModules(sortedMods)
    docSideBarSection(
      "Classes",
      sortedClasses,
      (c: Class) => sb.append(s"<a href='#class-${escUrl(c.sym.name)}'>${esc(c.sym.name)}</a>"),
    )
    docSideBarSection(
      "Effects",
      sortedEffs,
      (e: TypedAst.Effect) => sb.append(s"<a href='#eff-${escUrl(e.sym.name)}'>${esc(e.sym.name)}</a>"),
    )
    docSideBarSection(
      "Enums",
      sortedEnums,
      (e: TypedAst.Enum) => sb.append(s"<a href='#enum-${escUrl(e.sym.name)}'>${esc(e.sym.name)}</a>"),
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
    sb.append("</div>")
    sb.append("</nav>")

    sb.append("<main>")
    sb.append(s"<h1>${esc(moduleName(mod))}</h1>")
    docSection("Classes", sortedClasses, docClass)
    docSection("Effects", sortedEffs, docEffect)
    docSection("Enums", sortedEnums, docEnum)
    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedDefs, docDef)
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

  private def docSubModules(submodules: List[Symbol.ModuleSym])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (submodules.isEmpty) {
      return
    }

    sb.append("<h3>Modules</h3>")
    sb.append("<ul class='Modules'>")
    for (m <- submodules) {
      sb.append("<li>")
      sb.append(s"<a href='${esc(m.toString)}.html'>${esc(m.ns.last)}</a>")
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

    sb.append(s"<details ${if (open) "open" else ""}>")
    sb.append(s"<summary><h3>${esc(name)}</h3></summary>")
    for (e <- group) {
      docElt(e)
    }
    sb.append("</details>")
  }

  /**
    * Documents the given `Class`, `clazz`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docClass(clazz: Class)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='class-${esc(clazz.sym.name)}'>")
    docAnnotations(clazz.ann)
    docLink()
    sb.append("<code>")
    sb.append("<span class='keyword'>class</span> ")
    sb.append(s"<span class='name'>${esc(clazz.sym.name)}</span>")
    docTypeParams(List(clazz.tparam))
    docTypeConstraints(clazz.superClasses)
    sb.append("</code>")
    docSourceLocation(clazz.loc)
    docDoc(clazz.doc)
    docSubSection("Signatures", clazz.signatures.sortBy(_.sym.name), docSignature, open = true)
    docSubSection("Definitions", clazz.defs.sortBy(_.sym.name), docSignature)
    docSubSection("Instances", clazz.instances.sortBy(_.loc), docInstance)
    sb.append("</div>")
  }

  /**
    * Documents the given `Enum`, `enm`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEnum(enm: TypedAst.Enum)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='enum-${esc(enm.sym.name)}'>")
    docAnnotations(enm.ann)
    docLink()
    sb.append("<code>")
    sb.append("<span class='keyword'>enum</span> ")
    sb.append(s"<span class='name'>${esc(enm.sym.name)}</span>")
    docTypeParams(enm.tparams)
    docDerivations(enm.derives)
    sb.append("</code>")
    docSourceLocation(enm.loc)
    docCases(enm.cases.values.toList)
    docDoc(enm.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given `Effect`, `eff`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffect(eff: TypedAst.Effect)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='eff-${esc(eff.sym.name)}'>")
    docAnnotations(eff.ann)
    docLink()
    sb.append("<code>")
    sb.append("<span class='keyword'>eff</span> ")
    sb.append(s"<span class='name'>${esc(eff.sym.name)}</span>")
    sb.append("</code>")
    docSourceLocation(eff.loc)
    docSubSection("Operations", eff.ops, (o: TypedAst.Op) => docSpec(o.sym.name, o.spec), open = true)
    docDoc(eff.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given `TypeAlias`, `ta`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeAlias(ta: TypedAst.TypeAlias)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='ta-${esc(ta.sym.name)}'>")
    docLink()
    sb.append("<code>")
    sb.append("<span class='keyword'>type alias</span> ")
    sb.append(s"<span class='name'>${esc(ta.sym.name)}</span>")
    docTypeParams(ta.tparams)
    sb.append(" = ")
    docType(ta.tpe)
    sb.append("</code>")
    docSourceLocation(ta.loc)
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
    docLink()
    docSpec(defn.sym.name, defn.spec)
    sb.append("</div>")
  }

  /**
    * Documents the given `Sig`, `sig`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSignature(sig: TypedAst.Sig)(implicit flix: Flix, sb: StringBuilder): Unit =
    docSpec(sig.sym.name, sig.spec)

  /**
    * Documents the given `Spec`, `spec`, with the given `name`.
    * Shared by `Def` and `Sig`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSpec(name: String, spec: TypedAst.Spec)(implicit flix: Flix, sb: StringBuilder): Unit = {
    docAnnotations(spec.ann)
    sb.append(s"<code>")
    sb.append("<span class='keyword'>def</span> ")
    sb.append(s"<span class='name'>${esc(name)}</span>")
    docFormalParams(spec.fparams)
    sb.append(": ")
    docType(spec.retTpe)
    docEffectType(spec.eff)
    sb.append("</code>")
    docSourceLocation(spec.loc)
    docDoc(spec.doc)
  }

  /**
    * Documents the given `instance` of a class.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docInstance(instance: TypedAst.Instance)(implicit flix: Flix, sb: StringBuilder): Unit = {
    docAnnotations(instance.ann)
    sb.append("<code>")
    sb.append("<span class='keyword'>instance</span> ")
    docType(instance.tpe)
    docTypeConstraints(instance.tconstrs)
    sb.append("</code>")
    docSourceLocation(instance.loc)
    docDoc(instance.doc)
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
        case SimpleType.Tuple(fields) =>
          docList(fields) { t =>
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
    * Document the given `SourceLocation`, `loc`, in the form of a link.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSourceLocation(loc: SourceLocation)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<a class='source' target='_blank' href='${esc(createLink(loc))}'>Source</a>")
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
    * Appends a 'copy link' button the the given `StringBuilder`.
    * This creates a link to the ID of the direct parent, so this must be present.
    */
  private def docLink()(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<button class='copy-link' title='Requires JavaScript' aria-label='Copy link'>")
    inlineIcon("link")
    sb.append("</button> ")
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
    * Write the documentation output string of the `Module`, `mod`, into the output directory with a suitable name.
    */
  private def writeModule(mod: Module, output: String): Unit = {
    writeFile(s"${moduleFileName(mod)}.html", output.getBytes)
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
                            uses: List[Ast.UseOrImport],
                            submodules: List[Symbol.ModuleSym],
                            classes: List[Class],
                            enums: List[TypedAst.Enum],
                            effects: List[TypedAst.Effect],
                            typeAliases: List[TypedAst.TypeAlias],
                            defs: List[TypedAst.Def])

  /**
    * A represention of a class that's easier to work with while generating documention.
    */
  private case class Class(doc: Ast.Doc,
                           ann: Ast.Annotations,
                           mod: Ast.Modifiers,
                           sym: Symbol.ClassSym,
                           tparam: TypedAst.TypeParam,
                           superClasses: List[Ast.TypeConstraint],
                           assocs: List[TypedAst.AssocTypeSig],
                           signatures: List[TypedAst.Sig],
                           defs: List[TypedAst.Sig],
                           laws: List[TypedAst.Def],
                           instances: List[TypedAst.Instance],
                           loc: SourceLocation)
}
