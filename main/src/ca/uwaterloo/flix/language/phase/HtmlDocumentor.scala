/*
 * Copyright 2023 Holger Dal Mogensen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.{Flix, Version}
import ca.uwaterloo.flix.language.ast.shared.*
import ca.uwaterloo.flix.language.ast.{Kind, SourceLocation, Symbol, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatOptions, FormatType}
import ca.uwaterloo.flix.language.jvm.ClassDescs
import ca.uwaterloo.flix.tools.doc.HtmlHighlighter
import ca.uwaterloo.flix.util.LocalResource
import ca.uwaterloo.flix.util.collection.Nel
import org.commonmark.ext.gfm.tables.{TableCell, TablesExtension}
import org.commonmark.node.Node
import org.commonmark.parser.Parser
import org.commonmark.renderer.html.{AttributeProvider, HtmlRenderer}

import java.io.IOException
import java.net.URLEncoder
import java.nio.file.{Files, Path, Paths}
import java.util.regex.Pattern
import scala.annotation.tailrec

/**
  * A phase that emits a JSON file for library documentation.
  */
object HtmlDocumentor {

  /**
    * The "Pseudo-name" of the root namespace displayed on the pages.
    */
  private val RootNS: String = "Prelude"
  /**
    * The "Pseudo-name" of the root namespace used for its file name.
    */
  private val RootFileName: String = "index"

  /**
    * The path to the stylesheet, relative to the resources folder.
    */
  private val Stylesheet: String = "/doc/styles.css"

  /**
    * The path to the favicon, relative to the resources folder.
    */
  private val FavIcon: String = "/doc/favicon.png"

  /**
    * The path to the `index.js` script, relative to the resources folder.
    */
  private val Script: String = "/doc/index.js"

  /**
    * The path to the icon directory, relative to the resources folder.
    */
  private val Icons: String = "/doc/icons"

  /**
    * Matches an HTML comment, including any whitespace that follows it.
    */
  private val CommentPattern: Pattern = Pattern.compile("(?s)<!--.*?-->\\s*")

  /**
    * The icons of the documentation, as pairs of the CSS class that selects an icon and the name of
    * its SVG file, relative to [[Icons]].
    */
  private val IconClasses: List[(String, String)] = List(
    "back" -> "back",
    "close" -> "close",
    "dark" -> "darkMode",
    "light" -> "lightMode",
    "open" -> "menu",
  )

  /**
    * The symbols of the items that have a documentation page, grouped by kind.
    *
    * Used to decide whether a type constructor or trait name printed in a signature should be
    * rendered as a link: a symbol that has been filtered out of the documentation (e.g. because
    * it is not `pub`) has no page to link to, and its name should be printed as plain text
    * instead.
    *
    * A type alias has no page of its own -- it is documented on an anchor on its enclosing
    * module's page -- so `typeAliases` maps each alias symbol to that enclosing module, which is
    * enough to build the link.
    */
  case class DocumentedSymbols(
    traits: Set[Symbol.TraitSym],
    enums: Set[Symbol.EnumSym],
    structs: Set[Symbol.StructSym],
    effects: Set[Symbol.EffSym],
    typeAliases: Map[Symbol.TypeAliasSym, Symbol.ModuleSym],
  )

  /**
    * Generates the API documentation for `root` and writes it to `outputDir`.
    *
    * The declarations link to their source on the pages that [[HtmlHighlighter]] generates.
    */
  def run(root: TypedAst.Root, origin: Origin, outputDir: Path)(implicit flix: Flix): Unit = {
    val modulesRoot = splitModules(root)
    val filteredModulesRoot = filterModules(modulesRoot, origin)
    val pairedModulesRoot = pairModules(filteredModulesRoot)
    implicit val documented: DocumentedSymbols = collectDocumentedSymbols(filteredModulesRoot)

    visitMod(pairedModulesRoot, outputDir)

    HtmlHighlighter.run(root, origin, outputDir)(documentPage)

    writeDocFile("404.html", document404(), outputDir)

    writeAssets(outputDir)
  }

  /**
    * Documents the given `Module`, `mod`, and all of its contained items, writing the resulting HTML to disk.
    */
  private def visitMod(mod: Module, outputDir: Path)(implicit flix: Flix, documented: DocumentedSymbols): Unit = {
    writeDocFile(mod.fileName, documentModule(mod), outputDir)
    visitContents(mod, outputDir)
  }

  /**
    * Documents the given `Trait`, `trt`, and all of its contained items, writing the resulting HTML to disk.
    */
  private def visitTrait(trt: Trait, outputDir: Path)(implicit flix: Flix, documented: DocumentedSymbols): Unit = {
    writeDocFile(trt.fileName, documentTrait(trt), outputDir)
    trt.companionMod.foreach(visitContents(_, outputDir))
  }

  /**
    * Documents the given `Effect`, `eff`, and all of its contained items, writing the resulting HTML to disk.
    */
  private def visitEffect(eff: Effect, outputDir: Path)(implicit flix: Flix, documented: DocumentedSymbols): Unit = {
    writeDocFile(eff.fileName, documentEffect(eff), outputDir)
    eff.companionMod.foreach(visitContents(_, outputDir))
  }

  /**
    * Documents the given `Enum`, `enm`, and all of its contained items, writing the resulting HTML to disk.
    */
  private def visitEnum(enm: Enum, outputDir: Path)(implicit flix: Flix, documented: DocumentedSymbols): Unit = {
    writeDocFile(enm.fileName, documentEnum(enm), outputDir)
    enm.companionMod.foreach(visitContents(_, outputDir))
  }

  /**
    * Documents the given `Struct`, `struct`, and all of its contained items, writing the resulting HTML to disk.
    */
  private def visitStruct(struct: Struct, outputDir: Path)(implicit flix: Flix, documented: DocumentedSymbols): Unit = {
    writeDocFile(struct.fileName, documentStruct(struct), outputDir)
    struct.companionMod.foreach(visitContents(_, outputDir))
  }

  /**
    * Documents the items contained in the given `Module`, `mod`, but not the module itself,
    * writing the resulting HTML to disk.
    *
    * The items of a companion module are documented on the page of the item it belongs to,
    * so a companion module gets no page of its own.
    */
  private def visitContents(mod: Module, outputDir: Path)(implicit flix: Flix, documented: DocumentedSymbols): Unit = {
    mod.submodules.foreach(visitMod(_, outputDir))
    mod.traits.foreach(visitTrait(_, outputDir))
    mod.effects.foreach(visitEffect(_, outputDir))
    mod.enums.foreach(visitEnum(_, outputDir))
    mod.structs.foreach(visitStruct(_, outputDir))
  }

  /**
    * Get the shortest name of the module symbol, e.g. 'StdOut'.
    */
  private def moduleName(sym: Symbol.ModuleSym): String = sym.ns.lastOption.getOrElse(RootNS)

  /**
    * Get the fully qualified name of the module symbol, e.g. 'System.StdOut'.
    */
  private def moduleQualifiedName(sym: Symbol.ModuleSym): String = if (sym.isRoot) RootNS else sym.toString

  /**
    * Get the file name of the module symbol, e.g. 'System.StdOut.html'.
    */
  private def moduleFileName(sym: Symbol.ModuleSym): String = s"${if (sym.isRoot) RootFileName else sym.toString}.html"

  /**
    * Get the shortest name of the trait symbol, e.g. 'Foldable'.
    */
  private def traitName(sym: Symbol.TraitSym): String = sym.name

  /**
    * Get the fully qualified name of the trait symbol, e.g. 'Fixpoint.PredSymsOf'.
    */
  private def traitQualifiedName(sym: Symbol.TraitSym): String = sym.toString

  /**
    * Get the file name of the trait symbol, e.g. 'Fixpoint.PredSymsOf.html'.
    */
  private def traitFileName(sym: Symbol.TraitSym): String = s"${sym.toString}.html"

  /**
    * Get the shortest name of the effect symbol, e.g. 'StdOut'.
    */
  private def effectName(sym: Symbol.EffSym): String = sym.name

  /**
    * Get the fully qualified name of the effect symbol, e.g. 'System.StdOut'.
    */
  private def effectQualifiedName(sym: Symbol.EffSym): String = sym.toString

  /**
    * Get the file name of the effect symbol, e.g. 'System.StdOut.html'.
    */
  private def effectFileName(sym: Symbol.EffSym): String = s"${sym.toString}.html"

  /**
    * Get the shortest name of the enum symbol, e.g. 'StdOut'.
    */
  private def enumName(sym: Symbol.EnumSym): String = sym.name

  /**
    * Get the fully qualified name of the enum symbol, e.g. 'System.StdOut'.
    */
  private def enumQualifiedName(sym: Symbol.EnumSym): String = sym.toString

  /**
    * Get the file name of the enum symbol, e.g. 'System.StdOut.html'.
    */
  private def enumFileName(sym: Symbol.EnumSym): String = s"${sym.toString}.html"

  /**
    * Get the shortest name of the struct symbol, e.g. 'MutSet'.
    */
  private def structName(sym: Symbol.StructSym): String = sym.name

  /**
    * Get the fully qualified name of the struct symbol, e.g. 'MutSet.MutSet'.
    */
  private def structQualifiedName(sym: Symbol.StructSym): String = sym.toString

  /**
    * Get the file name of the struct symbol, e.g. 'MutSet.MutSet.html'.
    */
  private def structFileName(sym: Symbol.StructSym): String = s"${sym.toString}.html"

  /**
    * Splits the modules present in the root into a tree of `HtmlDocumentor.Module`s, making them easier to work with.
    *
    * Note: This function leaves all companion module fields empty.
    * Use `pairModules` to fill them in.
    */
  private def splitModules(root: TypedAst.Root): Module = {

    /**
      * Visits a module and all of its submodules
      */
    def visitMod(moduleSym: Symbol.ModuleSym, parent: Option[Symbol.ModuleSym]): Module = {
      val mod = root.modules(moduleSym)
      val uses = root.uses.get(moduleSym)

      var submodules: List[Symbol.ModuleSym] = Nil
      var traits: List[Trait] = Nil
      var effects: List[Effect] = Nil
      var enums: List[Enum] = Nil
      var structs: List[Struct] = Nil
      var typeAliases: List[TypedAst.TypeAlias] = Nil
      var defs: List[TypedAst.Def] = Nil
      mod.children.foreach {
        case sym: Symbol.ModuleSym => submodules = sym :: submodules
        case sym: Symbol.TraitSym =>
          traits = mkTrait(sym, moduleSym, root) :: traits
        case sym: Symbol.EffSym =>
          effects = mkEffect(sym, moduleSym, root) :: effects
        case sym: Symbol.EnumSym =>
          enums = mkEnum(sym, moduleSym, root) :: enums
        case sym: Symbol.StructSym =>
          structs = mkStruct(sym, moduleSym, root) :: structs
        case sym: Symbol.TypeAliasSym => typeAliases = root.typeAliases(sym) :: typeAliases
        case sym: Symbol.DefnSym => defs = root.defs(sym) :: defs
        case _ => // No op
      }

      Module(
        moduleSym,
        mod.doc,
        parent,
        uses,
        submodules.map(visitMod(_, Some(moduleSym))),
        traits,
        effects,
        enums,
        structs,
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
    val instances = root.instances.get(sym)

    Trait(decl, sigs, defs, instances, parent, None)
  }

  /**
    * Extracts all relevant information about the given `EffSym` from the root, into a `HtmlDocumentor.Effect`,
    * * leaving the companion module unpopulated.
    */
  private def mkEffect(sym: Symbol.EffSym, parent: Symbol.ModuleSym, root: TypedAst.Root): Effect = {
    val defaultHandler = root.defaultHandlers.find(_.handledSym == sym).map(_.handlerSym)
    Effect(root.effects(sym), defaultHandler, parent, None)
  }

  /**
    * Extracts all relevant information about the given `EnumSym` from the root, into a `HtmlDocumentor.Enum`,
    * * leaving the companion module unpopulated.
    */
  private def mkEnum(sym: Symbol.EnumSym, parent: Symbol.ModuleSym, root: TypedAst.Root): Enum = {
    val instances = instancesOf(root) {
      case TypeConstructor.Enum(s, _) => s == sym
      case _ => false
    }
    Enum(root.enums(sym), instances, parent, None)
  }

  /**
    * Extracts all relevant information about the given `StructSym` from the root, into a `HtmlDocumentor.Struct`,
    * leaving the companion module unpopulated.
    */
  private def mkStruct(sym: Symbol.StructSym, parent: Symbol.ModuleSym, root: TypedAst.Root): Struct = {
    val instances = instancesOf(root) {
      case TypeConstructor.Struct(s, _) => s == sym
      case _ => false
    }
    Struct(root.structs(sym), instances, parent, None)
  }

  /**
    * Returns the instances in `root` that should be included on the page of the type whose
    * type constructor satisfies `isType`.
    *
    * An instance is included if:
    *   1. It is for the type directly, e.g. `Eq[Boxed]`.
    *   1. It is for the type applied to some number of arguments, e.g. `Eq[Chain[a]] with Eq[a]`.
    */
  private def instancesOf(root: TypedAst.Root)(isType: TypeConstructor => Boolean): List[TypedAst.Instance] = {
    @tailrec
    def matches(tpe: Type): Boolean = tpe match {
      case Type.Cst(tc, _) => isType(tc)
      case Type.Apply(t, _, _) => matches(t)
      case _ => false
    }

    root.instances.values.filter(i => matches(i.tpe)).toList
  }

  /**
    * Filter the module, `mod`, and its children, removing all items and empty modules, which shouldn't appear in the documentation.
    */
  private def filterModules(mod: Module, origin: Origin): Module = {
    filterEmpty(filterContents(mod, origin))
  }

  /**
    * Returns the symbols of every trait, enum, struct, effect and type alias in `mod` and its
    * submodules, i.e. the items that will have a documentation page (or, for a type alias, an
    * anchor on its module's page) generated for them.
    *
    * This is used to determine whether a name printed in a signature should be rendered as a
    * link: an item that has been filtered out (e.g. because it is not `pub`) has no page to link
    * to, and its name should be printed as plain text instead.
    */
  private def collectDocumentedSymbols(mod: Module): DocumentedSymbols = {
    val subSyms = mod.submodules.map(collectDocumentedSymbols)

    val traits = mod.traits.iterator.map(_.decl.sym).toSet ++ subSyms.iterator.flatMap(_.traits)
    val enums = mod.enums.iterator.map(_.decl.sym).toSet ++ subSyms.iterator.flatMap(_.enums)
    val structs = mod.structs.iterator.map(_.decl.sym).toSet ++ subSyms.iterator.flatMap(_.structs)
    val effects = mod.effects.iterator.map(_.decl.sym).toSet ++ subSyms.iterator.flatMap(_.effects)
    val typeAliases = mod.typeAliases.iterator.map(t => t.sym -> mod.sym).toMap ++ subSyms.iterator.flatMap(_.typeAliases)

    DocumentedSymbols(traits, enums, structs, effects, typeAliases)
  }

  /**
    * Returns a tree of modules corresponding to the given input,
    * but with all contained items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterContents(mod: Module, origin: Origin): Module = mod match {
    case Module(sym, doc, parent, uses, submodules, traits, effects, enums, structs, typeAliases, defs) =>
      Module(
        sym,
        doc,
        parent,
        uses,
        submodules.map(m => filterContents(m, origin)),
        traits.filter(c => c.decl.mod.isPublic && isFrom(origin, c.decl.sym.loc)).map(c => filterTrait(c)),
        effects.filter(e => e.decl.mod.isPublic && isFrom(origin, e.decl.sym.loc)).map(e => filterEffect(e)),
        enums.filter(e => e.decl.mod.isPublic && isFrom(origin, e.decl.sym.loc)).map(e => filterEnum(e)),
        structs.filter(s => s.decl.mod.isPublic && isFrom(origin, s.decl.sym.loc)).map(s => filterStruct(s)),
        typeAliases.filter(t => t.mod.isPublic && isFrom(origin, t.sym.loc)),
        defs.filter(d => d.spec.mod.isPublic && isFrom(origin, d.sym.loc)),
      )
  }

  /**
    * Returns `true` if the declaration at `loc` comes from a source with the origin `origin`.
    *
    * A module carries no location of its own, so what is documented is decided per declaration.
    * A module whose declarations are all filtered out is then pruned by [[filterEmpty]], which is
    * how the bundled library and every dependency drop out of a project's documentation.
    */
  private def isFrom(origin: Origin, loc: SourceLocation): Boolean = loc.source.origin == origin

  /**
    * Returns a `Trait` corresponding to the given `trt`,
    * but with all items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterTrait(trt: Trait): Trait = trt match {
    case Trait(TypedAst.Trait(doc, ann, mod, sym, tparam, superTraits, assocs, _, loc), signatures, defs, instances, parent, _) =>
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
          loc
        ),
        signatures.filter(s => s.spec.mod.isPublic),
        defs.filter(d => d.spec.mod.isPublic),
        instances,
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
    case Effect(e, defaultHandler, parent, _) =>
      Effect(
        e,
        defaultHandler,
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
    case Enum(e, instances, parent, _) =>
      Enum(
        e,
        instances,
        parent,
        None,
      )
  }

  /**
    * Returns a `Struct` corresponding to the given `struct`,
    * but with all items that shouldn't appear in the documentation removed.
    *
    * Note: This function assumes that companion modules are unpopulated,
    * i.e. this should be called before `pairModules`.
    */
  private def filterStruct(struct: Struct): Struct = struct match {
    case Struct(s, instances, parent, _) =>
      Struct(
        s,
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
    /**
      * Recursively walks the module tree removing empty modules.
      */
    def visitMod(mod: Module): Option[Module] = mod match {
      case Module(sym, doc, parent, uses, submodules, traits, effects, enums, structs, typeAliases, defs) =>
        val filteredSubMods = submodules.flatMap(visitMod)

        val isEmpty =
          filteredSubMods.isEmpty &&
            traits.isEmpty &&
            effects.isEmpty &&
            enums.isEmpty &&
            structs.isEmpty &&
            typeAliases.isEmpty &&
            defs.isEmpty

        if (isEmpty) None
        else Some(
          Module(
            sym,
            doc,
            parent,
            uses,
            filteredSubMods,
            traits,
            effects,
            enums,
            structs,
            typeAliases,
            defs
          )
        )
    }

    visitMod(mod)
      .getOrElse(Module(
        mod.sym,
        mod.doc,
        None,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
        Nil,
      ))
  }

  /**
    * Get the given module tree, but with all companion modules paired to their respective items.
    */
  private def pairModules(mod: Module): Module = mod match {
    case Module(sym, doc, parent, uses, submodules, traits, effects, enums, structs, typeAliases, defs) =>

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
      val pairedStructs = structs.map { s =>
        val comp = visitedSubmodules.find(m => m.sym.ns.last == s.decl.sym.name)
        comp.foreach(c => companionMods = c :: companionMods)
        s.copy(companionMod = comp)
      }

      val filteredSubmodules = visitedSubmodules.filterNot(companionMods.contains)

      Module(
        sym,
        doc,
        parent,
        uses,
        filteredSubmodules,
        pairedTraits,
        pairedEffects,
        pairedEnums,
        pairedStructs,
        typeAliases,
        defs,
      )
  }

  /**
    * Documents the given `Module`, `mod`, returning a string of HTML.
    */
  private def documentModule(mod: Module)(implicit flix: Flix, documented: DocumentedSymbols): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedSubModules = mod.submodules.sortBy(_.name)
    val sortedTraits = mod.traits.sortBy(_.name)
    val sortedEnums = mod.enums.sortBy(_.name)
    val sortedStructs = mod.structs.sortBy(_.name)
    val sortedEffs = mod.effects.sortBy(_.name)
    val sortedTypeAliases = mod.typeAliases.sortBy(_.sym.name)
    val sortedDefs = mod.defs.sortBy(_.sym.name)

    sb.append(mkHead(mod.qualifiedName, mod.fileName))
    sb.append("<body class='no-script'>")

    docHeader()

    docSideBar(mod.parent) { () =>
      docSubModules(mod)
      docSideBarSection(
        "Traits",
        "traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        "effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        "enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Structs",
        "structs",
        sortedStructs,
        (s: Struct) => sb.append(s"<a href='${escUrl(s.fileName)}'>${esc(s.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        "type-aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        "definitions",
        sortedDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main id='main-content'>")
    docBreadcrumbs(mod.parent, mod.name)
    sb.append(s"<h1>${esc(mod.name)}</h1>")
    modDoc(mod.doc)
    docSummarySection("Modules", sortedSubModules, (m: Module) => m.doc)
    docSummarySection("Traits", sortedTraits, (t: Trait) => t.decl.doc)
    docSummarySection("Effects", sortedEffs, (e: Effect) => e.decl.doc)
    docSummarySection("Enums", sortedEnums, (e: Enum) => e.decl.doc)
    docSummarySection("Structs", sortedStructs, (s: Struct) => s.decl.doc)
    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedDefs, docDef)
    sb.append("</main>")

    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  /**
    * Documents the given `Trait`, `trt`, returning a string of HTML.
    */
  private def documentTrait(trt: Trait)(implicit flix: Flix, documented: DocumentedSymbols): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedAssocs = trt.decl.assocs.sortBy(_.sym.name)
    val sortedInstances = trt.instances.sortBy(i => FormatType.formatType(i.tpe))
    val sortedSigs = trt.signatures.sortBy(_.sym.name)
    val sortedTraitDefs = trt.defs.sortBy(_.sym.name)

    val mod = trt.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedStructs = mod.map(_.structs).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(trt.qualifiedName, trt.fileName))
    sb.append("<body class='no-script'>")

    docHeader()

    docSideBar(Some(trt.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Signatures",
        "signatures",
        sortedSigs,
        (s: TypedAst.Sig) => sb.append(s"<a href='#sig-${escUrl(s.sym.name)}'>${esc(s.sym.name)}</a>"),
      )
      docSideBarSection(
        "Trait Definitions",
        "trait-defs",
        sortedTraitDefs,
        (d: TypedAst.Sig) => sb.append(s"<a href='#sig-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
      docSideBarSection(
        "Traits",
        "traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        "effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        "enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Structs",
        "structs",
        sortedStructs,
        (s: Struct) => sb.append(s"<a href='${escUrl(s.fileName)}'>${esc(s.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        "type-aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Module Definitions",
        "definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main id='main-content'>")
    docBreadcrumbs(Some(trt.parent), trt.name)
    sb.append(s"<h1>${esc(trt.name)}</h1>")

    sb.append(s"<div class='box' id='main-box'>")
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
    mod.foreach(m => modDoc(m.doc))
    docSubSection("Associated Types", sortedAssocs, docAssoc)
    docCollapsableSubSection("Instances", sortedInstances, docInstance)
    sb.append("</div>")

    docSection("Signatures", sortedSigs, docSignature)
    docSection("Trait Definitions", sortedTraitDefs, docSignature)

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Module Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  /**
    * Documents the given `Effect`, `eff`, returning a string of HTML.
    */
  private def documentEffect(eff: Effect)(implicit flix: Flix, documented: DocumentedSymbols): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedOps = eff.decl.ops.sortBy(_.sym.name)

    val mod = eff.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedStructs = mod.map(_.structs).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(eff.qualifiedName, eff.fileName))
    sb.append("<body class='no-script'>")

    docHeader()

    docSideBar(Some(eff.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Operations",
        "operations",
        sortedOps, (o: TypedAst.Op) => sb.append(s"<a href='#op-${escUrl(o.sym.name)}'>${esc(o.sym.name)}</a>")
      )
      docSideBarSection(
        "Traits",
        "traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        "effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        "enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Structs",
        "structs",
        sortedStructs,
        (s: Struct) => sb.append(s"<a href='${escUrl(s.fileName)}'>${esc(s.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        "type-aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        "definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main id='main-content'>")
    docBreadcrumbs(Some(eff.parent), eff.name)
    sb.append(s"<h1>${esc(eff.name)}</h1>")

    sb.append(s"<div class='box'  id='main-box'>")
    docAnnotations(eff.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>eff</span> ")
    sb.append(s"<span class='name'>${esc(eff.name)}</span>")
    docTypeParams(eff.decl.tparams)
    sb.append("</code>")
    docActions(None, eff.decl.loc)
    sb.append("</div>")
    docDoc(eff.decl.doc)
    mod.foreach(m => modDoc(m.doc))
    docDefaultHandler(eff.defaultHandler)
    sb.append("</div>")

    docSection("Operations", sortedOps, docOp)

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  /**
    * Documents the given `Enum`, `enm`, returning a string of HTML.
    */
  private def documentEnum(enm: Enum)(implicit flix: Flix, documented: DocumentedSymbols): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedInstances = enm.instances.sortBy(_.trt.sym.name)

    val mod = enm.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedStructs = mod.map(_.structs).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(enm.qualifiedName, enm.fileName))
    sb.append("<body class='no-script'>")

    docHeader()

    docSideBar(Some(enm.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Traits",
        "traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        "effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        "enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Structs",
        "structs",
        sortedStructs,
        (s: Struct) => sb.append(s"<a href='${escUrl(s.fileName)}'>${esc(s.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        "type-aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        "definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main id='main-content'>")
    docBreadcrumbs(Some(enm.parent), enm.name)
    sb.append(s"<h1>${esc(enm.name)}</h1>")

    sb.append(s"<div class='box' id='main-box'>")
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
    mod.foreach(m => modDoc(m.doc))
    docCollapsableSubSection("Instances", sortedInstances, docInstance)
    sb.append("</div>")

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  /**
    * Documents the given `Struct`, `struct`, returning a string of HTML.
    */
  private def documentStruct(struct: Struct)(implicit flix: Flix, documented: DocumentedSymbols): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val sortedInstances = struct.instances.sortBy(_.trt.sym.name)

    val mod = struct.companionMod
    val sortedTraits = mod.map(_.traits).getOrElse(Nil).sortBy(_.name)
    val sortedEnums = mod.map(_.enums).getOrElse(Nil).sortBy(_.name)
    val sortedStructs = mod.map(_.structs).getOrElse(Nil).sortBy(_.name)
    val sortedEffs = mod.map(_.effects).getOrElse(Nil).sortBy(_.name)
    val sortedTypeAliases = mod.map(_.typeAliases).getOrElse(Nil).sortBy(_.sym.name)
    val sortedModuleDefs = mod.map(_.defs).getOrElse(Nil).sortBy(_.sym.name)

    sb.append(mkHead(struct.qualifiedName, struct.fileName))
    sb.append("<body class='no-script'>")

    docHeader()

    docSideBar(Some(struct.parent)) { () =>
      mod.foreach(docSubModules)
      docSideBarSection(
        "Traits",
        "traits",
        sortedTraits,
        (t: Trait) => sb.append(s"<a href='${escUrl(t.fileName)}'>${esc(t.name)}</a>"),
      )
      docSideBarSection(
        "Effects",
        "effects",
        sortedEffs,
        (e: Effect) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Enums",
        "enums",
        sortedEnums,
        (e: Enum) => sb.append(s"<a href='${escUrl(e.fileName)}'>${esc(e.name)}</a>"),
      )
      docSideBarSection(
        "Structs",
        "structs",
        sortedStructs,
        (s: Struct) => sb.append(s"<a href='${escUrl(s.fileName)}'>${esc(s.name)}</a>"),
      )
      docSideBarSection(
        "Type Aliases",
        "type-aliases",
        sortedTypeAliases,
        (t: TypedAst.TypeAlias) => sb.append(s"<a href='#ta-${escUrl(t.sym.name)}'>${esc(t.sym.name)}</a>"),
      )
      docSideBarSection(
        "Definitions",
        "definitions",
        sortedModuleDefs,
        (d: TypedAst.Def) => sb.append(s"<a href='#def-${escUrl(d.sym.name)}'>${esc(d.sym.name)}</a>"),
      )
    }

    sb.append("<main id='main-content'>")
    docBreadcrumbs(Some(struct.parent), struct.name)
    sb.append(s"<h1>${esc(struct.name)}</h1>")

    sb.append(s"<div class='box' id='main-box'>")
    docAnnotations(struct.decl.ann)
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>struct</span> ")
    sb.append(s"<span class='name'>${esc(struct.name)}</span>")
    docTypeParams(struct.decl.tparams)
    sb.append("</code>")
    docActions(None, struct.decl.loc)
    sb.append("</div>")
    docFields(struct.decl.fields.values.toList)
    docDoc(struct.decl.doc)
    mod.foreach(m => modDoc(m.doc))
    docCollapsableSubSection("Instances", sortedInstances, docInstance)
    sb.append("</div>")

    docSection("Type Aliases", sortedTypeAliases, docTypeAlias)
    docSection("Definitions", sortedModuleDefs, docDef)

    sb.append("</main>")

    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  /**
    * Documents the "page not found" error page, returning a string of HTML.
    */
  private def document404()(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    sb.append(mkHead("Page Not Found", "404.html"))
    sb.append("<body class='no-script'>")

    docHeader()

    sb.append("<nav aria-label='Sidebar navigation'></nav>")

    sb.append("<main id='main-content'>")
    sb.append("<h1>Page Not Found</h1>")
    sb.append("<p>There is no page at this address.</p>")
    sb.append("<p><a href='index.html'>Go to the documentation home</a>.</p>")
    sb.append("</main>")

    sb.append("</body>")

    sb.toString()
  }

  /**
    * Documents a page without a sidebar that is called `name` and has the given `content`,
    * returning a string of HTML.
    *
    * This is the frame of the pages that [[HtmlHighlighter]] generates.
    */
  private def documentPage(name: String, fileName: String, content: String): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    sb.append(mkHead(name, fileName))
    sb.append("<body class='no-script'>")

    docHeader()

    sb.append("<main id='main-content'>")
    sb.append(content)
    sb.append("</main>")

    sb.append("</body>")
    sb.append("</html>")

    sb.toString()
  }

  /**
    * Generates the string representing the head of the HTML document.
    */
  private def mkHead(name: String, fileName: String): String = {
    s"""<!doctype html><html lang='en'>
       |<head>
       |<meta charset='utf-8'>
       |<meta name='viewport' content='width=device-width,initial-scale=1'>
       |<meta name='description' content='API documentation for ${esc(name)} | The Flix Programming Language'>
       |<!-- Runs synchronously, before the stylesheets, so the reader's stored theme is applied before first paint; a deferred/module script (like index.js below) would run too late and cause a flash of the wrong theme. -->
       |<script>
       |(function () {
       |    try {
       |        var stored = localStorage.getItem('flix-html-docs:use-dark-theme');
       |        if (stored !== null) {
       |            document.documentElement.classList.add(stored === 'true' ? 'dark' : 'light');
       |        }
       |    } catch (e) {}
       |})();
       |</script>
       |<link href='https://fonts.googleapis.com/css?family=Fira+Code&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Oswald&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Noto+Sans&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Inter&display=swap' rel='stylesheet'>
       |<link href='https://fonts.googleapis.com/css?family=Open+Sans&display=swap' rel='stylesheet'>
       |<link href='styles.css' rel='stylesheet'>
       |<link href='favicon.png' rel='icon'>
       |<script type='module' src='./index.js'></script>
       |<title>Flix | ${esc(name)}</title>
       |</head>
    """.stripMargin
  }

  /**
    * Generate the page header.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docHeader()(implicit sb: StringBuilder): Unit = {
    sb.append("<a class='skip-link' href='#main-content'>Skip to main content</a>")

    sb.append("<header>")

    // The site branding is not marked up as a heading: each page's own `h1` (in `<main>`) is
    // the sole top-level heading, so this must not introduce an `h2` that precedes it.
    sb.append("<div class='flix'>")
    sb.append("<p class='site-title'><a href='index.html'>flix</a></p>")
    sb.append(s"<span class='version'>${Version.CurrentVersion}</span>")
    sb.append("</div>")

    sb.append("<div class='spacer' role='presentation'></div>")

    sb.append("<button id='theme-toggle' class='toggle' aria-label='Toggle Theme'>")
    docIcon("dark")
    docIcon("light")
    sb.append("</button>")

    // A `<label>` wrapping the checkbox gives the toggle a proper accessible name and an
    // interactive area that matches its whole visual hit target (rather than a bare `<div>`).
    sb.append("<label id='menu-toggle' class='toggle'>")
    sb.append("<input type='checkbox' aria-label='Toggle Navigation Menu'>")
    docIcon("open")
    docIcon("close")
    sb.append("</label>")

    sb.append("</header>")
  }

  /**
    * Generate the side bar with the contents specified by `docContents`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSideBar(parent: Option[Symbol.ModuleSym])(docContents: () => Unit)(implicit sb: StringBuilder): Unit = {
    sb.append("<nav aria-label='Sidebar navigation'>")
    // Visually hidden: gives the section `h3` headings below a proper `h2` ancestor within the
    // nav landmark's own heading structure, without changing how the sidebar looks.
    sb.append("<h2 class='visually-hidden'>Sidebar navigation</h2>")
    parent.map { p =>
      sb.append(s"<a class='back' href='${escUrl(moduleFileName(p))}'>")
      docIcon("back")
      sb.append(moduleName(p))
      sb.append("</a>")
    }
    docContents()
    sb.append("</nav>")
  }

  /**
    * Generate the breadcrumb trail of the page of the item called `name` in the module `parent`:
    * a link to each enclosing module, outermost first, followed by `name` itself.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `parent` is `None`, i.e. the page is that of the root module, nothing will be generated.
    */
  private def docBreadcrumbs(parent: Option[Symbol.ModuleSym], name: String)(implicit sb: StringBuilder): Unit = {
    parent.foreach { p =>
      sb.append("<div class='breadcrumbs'>")
      for (m <- p.ns.inits.toList.reverse.map(Symbol.mkModuleSym)) {
        sb.append(s"<a href='${escUrl(moduleFileName(m))}'>${esc(moduleName(m))}</a> / ")
      }
      sb.append(s"<span>${esc(name)}</span>")
      sb.append("</div>")
    }
  }

  /**
    * Documents a section in the side bar, (Modules, Traits, Enums, etc.), containing a `group` of items.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name     The name of the section, e.g. "Modules".
    * @param cssClass A stable, purpose-named CSS class identifying the kind of items in `group`
    *                 (e.g. "traits"), used to style the list independently of `name`.
    * @param group    The list of items in the section, in the order that they should appear.
    * @param docElt   A function taking a single item from `group` and generating the corresponding HTML string.
    *                 Note that they will each be wrapped in an `<li>` tag.
    */
  private def docSideBarSection[T](name: String, cssClass: String, group: List[T], docElt: T => Unit)(implicit sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<h3><a href='#${escUrl(name.replace(' ', '-'))}'>${esc(name)}</a></h3>")
    sb.append(s"<ul class='sidebar-list sidebar-list-${esc(cssClass)}'>")
    for (e <- group) {
      sb.append("<li>")
      docElt(e)
      sb.append("</li>")
    }
    sb.append("</ul>")
  }

  private def docSubModules(parentMod: Module)(implicit sb: StringBuilder): Unit = {
    val sortedItems = parentMod.submodules.sortBy(_.name)

    if (sortedItems.isEmpty) {
      return
    }

    sb.append("<h3>Modules</h3>")
    sb.append("<ul class='sidebar-list sidebar-list-modules'>")
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
  private def docSection[T](name: String, group: List[T], docElt: T => Unit)(implicit sb: StringBuilder): Unit = {
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
    * Documents a summary section, (Traits, Effects, Enums, Structs), in the main content column,
    * containing a `group` of items.
    *
    * Unlike [[docSection]], each item is summarized as a single table row containing its name,
    * linked to its own page, and the first paragraph of its documentation comment. This mirrors
    * how e.g. rustdoc and Javadoc summarize the members of a module/package.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the section, e.g. "Traits". This name will also be the id of the section.
    * @param group  The list of items in the section, in the order that they should appear.
    * @param getDoc A function returning the documentation comment of an item.
    */
  private def docSummarySection[T <: Item](name: String, group: List[T], getDoc: T => Doc)(implicit sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<section id='${name.replace(' ', '-')}'>")
    sb.append(s"<h2>$name</h2>")
    sb.append("<table class='summary-table'>")
    for (e <- group) {
      sb.append("<tr>")
      sb.append(s"<td><a class='name' href='${escUrl(e.fileName)}'>${esc(e.name)}</a></td>")
      sb.append(s"<td>${esc(summaryText(getDoc(e)))}</td>")
      sb.append("</tr>")
    }
    sb.append("</table>")
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
  private def docSubSection[T](name: String, group: List[T], docElt: T => Unit)(implicit sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<section class='subsection'>")
    sb.append(s"<h2>${esc(name)}</h2>")
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
  private def docCollapsableSubSection[T](name: String, group: List[T], docElt: T => Unit)(implicit sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append(s"<details class='subsection'>")
    sb.append(s"<summary><h2>${esc(name)}</h2></summary>")
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
  private def docTypeAlias(ta: TypedAst.TypeAlias)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='ta-${esc(ta.sym.name)}'>")
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>type alias</span> ")
    sb.append(s"<span class='name'>${esc(ta.sym.name)}</span>")
    docTypeParams(ta.tparams)
    sb.append(" = ")
    docType(ta.tpe)
    sb.append("</code>")
    docActions(Some(s"ta-${ta.sym.name}"), ta.loc)
    sb.append("</div>")
    docDoc(ta.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given `Def`, `defn`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDef(defn: TypedAst.Def)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='def-${esc(defn.sym.name)}'>")
    docSpec(defn.sym.name, defn.spec, defn.loc, Some(s"def-${defn.sym.name}"))
    sb.append("</div>")
  }

  /**
    * Documents the given `Sig`, `sig`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSignature(sig: TypedAst.Sig)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='sig-${esc(sig.sym.name)}'>")
    docSpec(sig.sym.name, sig.spec, sig.loc, Some(s"sig-${sig.sym.name}"))
    sb.append("</div>")
  }

  /**
    * Documents the given `Op`, `op`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docOp(op: TypedAst.Op)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append(s"<div class='box' id='op-${esc(op.sym.name)}'>")
    docSpec(op.sym.name, op.spec, op.loc, Some(s"op-${op.sym.name}"))
    sb.append("</div>")
  }

  /**
    * Documents the given `Spec`, `spec`, with the given `name`.
    * Shared by `Def` and `Sig`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSpec(name: String, spec: TypedAst.Spec, loc: SourceLocation, linkId: Option[String])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
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
    docActions(linkId, loc)
    sb.append("</div>")
    docDoc(spec.doc)
  }

  /**
    * Documents the given associated type of a trait.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docAssoc(assoc: TypedAst.AssocTypeSig)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append("<div>")
    sb.append("<div class='decl'>")
    sb.append("<code>")
    sb.append("<span class='keyword'>type</span> ")
    sb.append(s"<span class='name'>${esc(assoc.sym.name)}</span>")
    sb.append(": ")
    docKind(assoc.kind)
    assoc.tpe.foreach { t =>
      sb.append(" = ")
      docTypeOrEffect(t)
    }
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
  private def docInstance(instance: TypedAst.Instance)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
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
    docEqualityConstraints(instance.econstrs)
    sb.append("</code>")
    docActions(None, instance.loc)
    sb.append("</div>")
    docAssocDefs(instance.assocs)
    docDoc(instance.doc)
    sb.append("</div>")
  }

  /**
    * Documents the given list of `AssocTypeDef`s of an instance, e.g. `type Elm = Char`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `assocs` is empty, nothing will be generated.
    */
  private def docAssocDefs(assocs: List[TypedAst.AssocTypeDef])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (assocs.isEmpty) {
      return
    }

    sb.append("<div class='assocs'>")
    for (a <- assocs.sortBy(_.loc)) {
      sb.append("<code>")
      sb.append("<span class='keyword'>type</span> ")
      sb.append(s"<span class='name'>${esc(a.symUse.sym.name)}</span>")
      sb.append(" = ")
      docTypeOrEffect(a.tpe)
      sb.append("</code>")
    }
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
  private def docTraitConstraints(tconsts: List[TraitConstraint])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (tconsts.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")
    docList(tconsts.sortBy(_.loc)) { t =>
      docTraitName(t.symUse.sym)
      sb.append("[")
      docType(t.arg)
      sb.append("]")
    }
    sb.append("</span>")
  }

  /**
    * Document the name of the given trait symbol, creating a link to the trait's documentation
    * page if it has one, i.e. if it is in `documented.traits`. Otherwise, the name is documented
    * as plain text, since linking to it would result in a dead link (e.g. for a non-`pub` trait).
    */
  private def docTraitName(sym: Symbol.TraitSym)(implicit documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (documented.traits.contains(sym)) {
      sb.append(s"<a class='tpe-constraint' href='${escUrl(traitFileName(sym))}' title='trait ${esc(traitName(sym))}'>")
      sb.append(esc(sym.name))
      sb.append("</a>")
    } else {
      sb.append(s"<span class='tpe-constraint' title='trait ${esc(traitName(sym))}'>")
      sb.append(esc(sym.name))
      sb.append("</span>")
    }
  }

  /**
    * Documents the given list of `EqualityConstraint`s, `econsts`.
    * E.g. "where C.T[a] ~ String".
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `econsts` is empty, nothing will be generated.
    */
  private def docEqualityConstraints(econsts: List[TypedAst.EqualityConstraint])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (econsts.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>where</span> ")
    docList(econsts.sortBy(_.loc)) { e =>
      e.tpe1 match {
        case Type.AssocType(cst, arg, _, _) =>
          docTraitName(cst.sym.trt)
          sb.append(".")
          sb.append(esc(cst.sym.name))
          sb.append("[")
          docType(arg)
          sb.append("] ~ ")
          docType(e.tpe2)
        case _ =>
          docType(e.tpe1)
          sb.append(" ~ ")
          docType(e.tpe2)
      }
    }
    sb.append("</span>")
  }

  /**
    * Documents the given `Derivations`s, `derives`.
    * E.g. "with ToString".
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `derives` contains no elements, nothing will be generated.
    */
  private def docDerivations(derives: Derivations)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (derives.traits.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")
    docList(derives.traits.sortBy(_.loc)) { t =>
      docTraitName(t.sym)
    }
    sb.append("</span>")
  }

  /**
    * Documents the given list of `Case`s of an enum.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docCases(cases: List[TypedAst.Case])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append("<div class='cases'>")
    for (c <- cases.sortBy(_.loc)) {
      sb.append("<code>")
      sb.append("<span class='keyword'>case</span> ")
      sb.append(s"<span class='case-tag'>${esc(c.sym.name)}</span>")

      c.tpes match {
        case Nil => // Nothing
        case elms =>
          sb.append("(")
          docList(elms)(docType)
          sb.append(")")
      }

      sb.append("</code>")
    }
    sb.append("</div>")
  }

  /**
    * Documents the given list of `StructField`s of a struct.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `fields` is empty, nothing will be generated.
    */
  private def docFields(fields: List[TypedAst.StructField])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (fields.isEmpty) {
      return
    }

    sb.append("<div class='fields'>")
    for (f <- fields.sortBy(_.loc)) {
      sb.append("<code>")
      if (f.mod.isMutable) {
        sb.append("<span class='keyword'>mut</span> ")
      }
      sb.append(s"<span>${esc(f.sym.name)}</span>: ")
      docType(f.tpe)
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
  private def docFormalParams(fparams: Nel[TypedAst.FormalParam])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append("<span class='fparams'>(")
    fparams match {
      case Nel(TypedAst.FormalParam(_, Type.Cst(TypeConstructor.Unit, _), _, _, _), Nil) =>
      // For a function declared with zero formal parameters,
      // the compiler will introduce a single parameter of the unit type
      case _ =>
        docList(fparams.toList.sortBy(_.loc)) { p =>
          sb.append(s"<span><span>${esc(p.bnd.sym.text)}</span>: ")
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
  private def docAnnotations(anns: Annotations)(implicit sb: StringBuilder): Unit = {
    val visible = anns.annotations.filter(isPublicAnnotation)
    if (visible.isEmpty) {
      return
    }

    sb.append("<code class='annotations'>")
    for (a <- visible) {
      sb.append(s"<span class='annotation'>${esc(a.toString)}</span> ")
    }
    sb.append("</code>")
  }

  /**
    * Returns `true` if `ann` is meaningful to a caller of the API and should be documented.
    *
    * Annotations that only describe compiler-internal bookkeeping (e.g. lowering targets or
    * test-framework wiring) are hidden.
    */
  private def isPublicAnnotation(ann: Annotation): Boolean = ann match {
    case Annotation.Deprecated(_) => true
    case Annotation.Experimental(_) => true
    case Annotation.Parallel(_) => true
    case Annotation.ParallelWhenPure(_) => true
    case Annotation.Lazy(_) => true
    case Annotation.LazyWhenPure(_) => true
    case Annotation.Terminates(_) => true
    case Annotation.DefaultHandler(_) => true
    case Annotation.Inline(_) => true
    case Annotation.DontInline(_) => true
    case Annotation.TailRecursive(_) => true

    case Annotation.CompileTest(_) => false
    case Annotation.LoweringTargetChannel(_) => false
    case Annotation.LoweringTargetDatalog(_) => false
    case Annotation.Skip(_) => false
    case Annotation.Test(_) => false
    case Annotation.Error(_, _) => false
  }

  /**
    * Appends a 'copy link' button the the given `StringBuilder`.
    * This creates a link to the given ID on the current URL.
    *
    * The button is marked by a section sign rather than an inlined SVG icon. There is one of these
    * buttons per documented element, so an inlined icon would dominate the size of the page.
    */
  private def docLink(id: String)(implicit sb: StringBuilder): Unit = {
    sb.append(s"<a href='#${escUrl(id)}' class='copy-link'>&sect;</a> ")
  }

  /**
    * Document the given `SourceLocation`, `loc`, in the form of a link, if it has one.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSourceLocation(loc: SourceLocation)(implicit sb: StringBuilder): Unit = {
    createLink(loc).foreach(link => sb.append(s"<a class='source' href='$link'>Source</a>"))
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
    * Documents the default handler of an effect, `handler`, if it has one, as a
    * "Default Handler" subsection that links to the definition.
    *
    * A default handler is public and declared in the companion module of its effect,
    * so the link points at the definition on the effect's own page.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDefaultHandler(handler: Option[Symbol.DefnSym])(implicit sb: StringBuilder): Unit = {
    handler.foreach { sym =>
      val page = moduleFileName(Symbol.mkModuleSym(sym.namespace))
      sb.append("<section class='subsection default-handler'>")
      sb.append("<h2>Default Handler</h2>")
      sb.append(s"<div><code><a href='${escUrl(page)}#def-${escUrl(sym.name)}'>${esc(sym.name)}</a></code></div>")
      sb.append("</section>")
    }
  }

  /**
    * Document the the given `doc`, while parsing any markdown.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDoc(doc: Doc)(implicit sb: StringBuilder): Unit = {
    renderDoc(doc, "doc")
  }

  /**
    * Renders a module-level [[Doc]] using the `mod-doc` CSS class.
    *
    * Module-level documentation uses its own class so it can carry distinct
    * typography from item-level docs (e.g. larger font, different family).
    */
  private def modDoc(doc: Doc)(implicit sb: StringBuilder): Unit = {
    renderDoc(doc, "mod-doc")
  }

  private def renderDoc(doc: Doc, cls: String)(implicit sb: StringBuilder): Unit = {
    val text = doc.text
    if (text.isBlank) {
      return
    }

    val extensions = java.util.List.of(TablesExtension.create())
    val parser = Parser.builder().extensions(extensions).build()
    val node = parser.parse(text)
    val renderer = HtmlRenderer.builder()
      .extensions(extensions)
      .escapeHtml(true)
      .attributeProviderFactory(_ => TableCellAlignment)
      .build()
    val html = renderer.render(node)

    sb.append(s"<div class='$cls'>")
    sb.append(html)
    sb.append("</div>")
  }

  /**
    * Returns the first paragraph of the given `doc`, as plain text (i.e. any markdown syntax is
    * not rendered, only stripped of its surrounding paragraph/line structure), collapsed onto a
    * single line.
    *
    * This is intended for use in short summaries, mirroring how e.g. rustdoc and Javadoc derive
    * a summary from a doc comment.
    *
    * If `doc` is empty, the empty string is returned.
    */
  private def summaryText(doc: Doc): String = {
    val text = doc.text
    if (text.isBlank) {
      return ""
    }

    // Markdown paragraphs are separated by a blank line; only the first paragraph is relevant.
    val firstParagraph = text.split("\r?\n\\s*\r?\n", 2).head

    // Collapse the paragraph's (possibly soft-wrapped) lines into a single line.
    firstParagraph.linesIterator.map(_.trim).filter(_.nonEmpty).mkString(" ")
  }

  /**
    * Replaces the obsolete `align` attribute that the tables extension puts on table cells with an
    * `align-left`, `align-center`, or `align-right` class, which the stylesheet maps to `text-align`.
    */
  private object TableCellAlignment extends AttributeProvider {
    override def setAttributes(node: Node, tagName: String, attributes: java.util.Map[String, String]): Unit = node match {
      case _: TableCell =>
        val align = attributes.remove("align")
        if (align != null) {
          attributes.put("class", s"align-$align")
        }
      case _ => ()
    }
  }


  /**
    * Document the given `Type`, `tpe`, from its type tree, rather than from a single
    * pre-formatted string.
    *
    * Every type constructor that has a documentation page (an enum, a struct, an effect or a
    * type alias -- see [[DocumentedSymbols]]) becomes a link to that page, the same way a trait
    * name in a `with` clause already does. A type constructor with no page (a built-in like
    * `Int32` or `List`, if it has been filtered out of the documentation) is printed as plain
    * text. A type variable is printed with its own `type-var` class, distinct from a type
    * constructor. An effect nested anywhere in `tpe` -- e.g. inside a parameter's own function
    * type -- is printed with the same `effect` class a top-level effect gets, via
    * [[docEffectFormula]], rather than being flattened into the surrounding type's plain text.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docType(tpe: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    sb.append("<span class='type'>")
    docTypeTree(tpe)
    sb.append("</span>")
  }

  /**
    * Document the given `Type`, `tpe`, as an effect if it is of kind `Eff` and as a type otherwise.
    *
    * An effect is written as it would be in source, e.g. `{}` rather than `Pure`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeOrEffect(tpe: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (tpe.kind != Kind.Eff) {
      docType(tpe)
    } else if (isPureEffect(tpe)) {
      sb.append("<span class='effect'>{}</span>")
    } else {
      docEffectFormula(tpe)
    }
  }

  /**
    * Document the the given `Kind`, `kind`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docKind(kind: Kind)(implicit sb: StringBuilder): Unit = {
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
  private def docEffectType(eff: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (isPureEffect(eff)) {
      return
    }
    sb.append(" \\ ")
    docEffectFormula(eff)
  }

  /**
    * Returns `true` if `tpe` is structurally the empty effect set, written with no combinators,
    * e.g. the effect of a pure function.
    */
  private def isPureEffect(tpe: Type): Boolean = tpe.typeArguments.isEmpty && (tpe.baseType match {
    case Type.Cst(TypeConstructor.Pure, _) => true
    case _ => false
  })

  /**
    * Recursively documents the given `Type`, `tpe0`, one HTML element per type constructor and
    * type variable, linking every type constructor that has a documentation page.
    *
    * This mirrors the constructor cases that [[DisplayType.fromWellKindedType]] and
    * [[FormatType]] handle when formatting a type as a single string, but emits an HTML element
    * (a link when the constructor has a page, otherwise a plain-text span) per constructor
    * instead of one flat string. A handful of exotic constructors that essentially never appear
    * in a documented signature (unresolved JVM member types, extensible variants, restrictable
    * case sets, and the like) are not walked individually; rather than dropping them, they fall
    * back to the shared formatter for just that subtree, via [[docFallback]].
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeTree(tpe0: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    val args = tpe0.typeArguments
    tpe0.baseType match {
      case Type.Var(sym, _) =>
        docTypeVar(sym)
        docTypeArgsInBrackets(args)

      case Type.Alias(symUse, _, _, _) =>
        docAliasName(symUse.sym)
        docTypeArgsInBrackets(args)

      case Type.AssocType(symUse, arg, _, _) =>
        docTraitName(symUse.sym.trt)
        sb.append(".")
        sb.append(esc(symUse.sym.name))
        sb.append("[")
        docTypeTree(arg)
        args.foreach { a => sb.append(", "); docTypeTree(a) }
        sb.append("]")

      case Type.Cst(tc, _) =>
        docTypeConstructor(tc, args, tpe0)

      case _ =>
        // Type.JvmToType, Type.JvmToEff, Type.UnresolvedJvmType, or (should not happen) a bare
        // Type.Apply: fall back to the shared formatter rather than dropping the information.
        docFallback(tpe0)
    }
  }

  /**
    * Documents the given `TypeConstructor`, `tc`, applied to the given `args`, as part of
    * [[docTypeTree]]. `whole` is the original type, kept around only so a case that isn't fully
    * handled here can fall back to formatting it as a whole via [[docFallback]].
    */
  private def docTypeConstructor(tc: TypeConstructor, args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = tc match {
    case TypeConstructor.Void => docPlainTypeName("Void"); docTypeArgsInBrackets(args)
    case TypeConstructor.Unit => docPlainTypeName("Unit"); docTypeArgsInBrackets(args)
    case TypeConstructor.Null => docPlainTypeName("Null"); docTypeArgsInBrackets(args)
    case TypeConstructor.Bool => docPlainTypeName("Bool"); docTypeArgsInBrackets(args)
    case TypeConstructor.Char => docPlainTypeName("Char"); docTypeArgsInBrackets(args)
    case TypeConstructor.Float32 => docPlainTypeName("Float32"); docTypeArgsInBrackets(args)
    case TypeConstructor.Float64 => docPlainTypeName("Float64"); docTypeArgsInBrackets(args)
    case TypeConstructor.BigDecimal => docPlainTypeName("BigDecimal"); docTypeArgsInBrackets(args)
    case TypeConstructor.Int8 => docPlainTypeName("Int8"); docTypeArgsInBrackets(args)
    case TypeConstructor.Int16 => docPlainTypeName("Int16"); docTypeArgsInBrackets(args)
    case TypeConstructor.Int32 => docPlainTypeName("Int32"); docTypeArgsInBrackets(args)
    case TypeConstructor.Int64 => docPlainTypeName("Int64"); docTypeArgsInBrackets(args)
    case TypeConstructor.BigInt => docPlainTypeName("BigInt"); docTypeArgsInBrackets(args)
    case TypeConstructor.Str => docPlainTypeName("String"); docTypeArgsInBrackets(args)
    case TypeConstructor.Regex => docPlainTypeName("Regex"); docTypeArgsInBrackets(args)
    case TypeConstructor.Sender => docPlainTypeName("Sender"); docTypeArgsInBrackets(args)
    case TypeConstructor.Receiver => docPlainTypeName("Receiver"); docTypeArgsInBrackets(args)
    case TypeConstructor.Lazy => docPlainTypeName("Lazy"); docTypeArgsInBrackets(args)
    case TypeConstructor.Array => docPlainTypeName("Array"); docTypeArgsInBrackets(args)
    case TypeConstructor.Vector => docPlainTypeName("Vector"); docTypeArgsInBrackets(args)
    case TypeConstructor.RegionToStar => docPlainTypeName("Region"); docTypeArgsInBrackets(args)
    case TypeConstructor.True => docPlainTypeName("true")
    case TypeConstructor.False => docPlainTypeName("false")
    case TypeConstructor.Pure => docPlainTypeName("Pure")
    case TypeConstructor.Univ => docPlainTypeName("Univ")

    case TypeConstructor.Region(sym) =>
      // A region is itself an effect (the capability to use its scoped resources), so it gets
      // the same role as any other effect.
      sb.append(s"<span class='effect'>${esc(sym.text)}</span>")

    case TypeConstructor.Native(desc, _) =>
      docPlainTypeName(ClassDescs.canonicalNameOf(desc))
      docTypeArgsInBrackets(args)

    case TypeConstructor.RestrictableEnum(sym, _) =>
      // Restrictable enums are not tracked in `DocumentedSymbols`, so their names are always
      // printed as plain text. This is a rare, experimental feature; extending the link tracking
      // to cover it is left for a follow-up.
      docPlainTypeName(sym.name)
      docTypeArgsInBrackets(args)

    case TypeConstructor.Enum(sym, _) =>
      docDocumentedTypeName(documented.enums.contains(sym), enumFileName(sym), "enum", enumName(sym))
      docTypeArgsInBrackets(args)

    case TypeConstructor.Struct(sym, _) =>
      docDocumentedTypeName(documented.structs.contains(sym), structFileName(sym), "struct", structName(sym))
      docTypeArgsInBrackets(args)

    case TypeConstructor.Effect(sym, _) =>
      docEffectSymName(sym)
      docTypeArgsInBrackets(args)

    case TypeConstructor.Tuple(arity) => docTuple(arity, args, whole)
    case TypeConstructor.Relation(arity) => docParenWrapped("Relation", arity, args, whole)
    case TypeConstructor.Lattice(arity) => docLattice(arity, args, whole)
    case TypeConstructor.Arrow(arity) => docArrow(arity, args, whole)
    case TypeConstructor.Record => docRecord(args, whole)
    case TypeConstructor.Schema => docSchema(args, whole)

    case _ =>
      // Every other constructor (record/schema rows seen outside of Record/Schema, extensible
      // variants, restrictable case sets, resolved JVM members, ArrowWithoutEffect,
      // ArrayWithoutRegion, RegionWithoutRegion, AnyType, Error, and any future addition): these
      // essentially never appear in a type written in a signature, so rather than a bespoke
      // renderer for each, fall back to the shared formatter for just this subtree.
      docFallback(whole)
  }

  /**
    * Documents a type variable, `sym`, with its own `type-var` class, distinct from a type
    * constructor.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeVar(sym: Symbol.KindedTypeVarSym)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='type-var'>")
    sb.append(esc(varName(sym)))
    sb.append("</span>")
  }

  /**
    * Returns the name of the given type variable symbol, `sym`, as it should be printed: its
    * source name if one is known and the format options ask for names, or an id-based name
    * derived from its kind otherwise.
    *
    * This mirrors the `Var` case of [[FormatType]]'s own visitor.
    */
  private def varName(sym: Symbol.KindedTypeVarSym)(implicit flix: Flix): String = {
    val idBased = sym.kind match {
      case Kind.Wild => "_" + sym.id.toString
      case Kind.WildCaseSet => "_c" + sym.id.toString
      case Kind.Star => "t" + sym.id
      case Kind.Eff => "e" + sym.id
      case Kind.Bool => "b" + sym.id
      case Kind.RecordRow => "r" + sym.id
      case Kind.SchemaRow => "s" + sym.id
      case Kind.Predicate => "'" + sym.id.toString
      case Kind.Jvm => "j" + sym.id.toString
      case Kind.CaseSet(_) => "c" + sym.id.toString
      case Kind.Arrow(_, _) => "'" + sym.id.toString
      case Kind.Error => "err" + sym.id.toString
    }
    flix.getFormatOptions.varNames match {
      case FormatOptions.VarName.IdBased => idBased
      case FormatOptions.VarName.NameBased => sym.text match {
        case VarText.Absent => idBased
        case VarText.SourceText(s) => s
      }
    }
  }

  /**
    * Documents the given plain (non-linkable) type constructor name, `name`, e.g. a built-in
    * like `Int32` that has no documentation page.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docPlainTypeName(name: String)(implicit sb: StringBuilder): Unit = {
    sb.append(s"<span class='type'>${esc(name)}</span>")
  }

  /**
    * Documents the name of a documentable type constructor (an enum or a struct), creating a
    * link to its documentation page if `isDocumented`, the same way [[docTraitName]] does for a
    * trait. Otherwise, the name is documented as plain text, since linking to it would result in
    * a dead link (e.g. for a non-`pub` enum).
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDocumentedTypeName(isDocumented: Boolean, fileName: String, label: String, shortName: String)(implicit sb: StringBuilder): Unit = {
    if (isDocumented) {
      sb.append(s"<a class='type' href='${escUrl(fileName)}' title='$label ${esc(shortName)}'>")
      sb.append(esc(shortName))
      sb.append("</a>")
    } else {
      sb.append(s"<span class='type' title='$label ${esc(shortName)}'>")
      sb.append(esc(shortName))
      sb.append("</span>")
    }
  }

  /**
    * Documents the name of the given type alias symbol, `sym`, creating a link to the anchor on
    * its module's page if it has one, i.e. if it is in `documented.typeAliases`. Otherwise, the
    * name is documented as plain text.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docAliasName(sym: Symbol.TypeAliasSym)(implicit documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    documented.typeAliases.get(sym) match {
      case Some(modSym) =>
        val page = moduleFileName(modSym)
        sb.append(s"<a class='type' href='${escUrl(page)}#ta-${escUrl(sym.name)}' title='type alias ${esc(sym.name)}'>")
        sb.append(esc(sym.name))
        sb.append("</a>")
      case None =>
        sb.append(s"<span class='type' title='type alias ${esc(sym.name)}'>")
        sb.append(esc(sym.name))
        sb.append("</span>")
    }
  }

  /**
    * Documents the name of the given effect symbol, `sym`, creating a link to its documentation
    * page if it has one, i.e. if it is in `documented.effects`. Otherwise, the name is documented
    * as plain text.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffectSymName(sym: Symbol.EffSym)(implicit documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (documented.effects.contains(sym)) {
      sb.append(s"<a class='effect' href='${escUrl(effectFileName(sym))}' title='effect ${esc(effectName(sym))}'>")
      sb.append(esc(sym.name))
      sb.append("</a>")
    } else {
      sb.append(s"<span class='effect' title='effect ${esc(effectName(sym))}'>")
      sb.append(esc(sym.name))
      sb.append("</span>")
    }
  }

  /**
    * Documents the given list of type arguments, `args`, wrapped in `[]`, if there are any.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeArgsInBrackets(args: List[Type])(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (args.nonEmpty) {
      sb.append("[")
      docList(args)(docTypeTree)
      sb.append("]")
    }
  }

  /**
    * Returns `true` if a type headed by `tc` needs to be parenthesized when it appears somewhere
    * that isn't innately delimited (e.g. as the return type of a function). Mirrors the
    * `isDelimited` cases of [[FormatType]] that can actually appear in a well-kinded signature.
    */
  private def needsParens(tpe: Type): Boolean = tpe.baseType match {
    case Type.Cst(TypeConstructor.Arrow(_), _) => true
    case Type.Cst(TypeConstructor.Not | TypeConstructor.And | TypeConstructor.Or, _) => true
    case _ => false
  }

  /**
    * Documents `tpe`, parenthesizing it first if [[needsParens]] says it would otherwise be
    * ambiguous.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDelimitedType(tpe: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (needsParens(tpe)) {
      sb.append("(")
      docTypeTree(tpe)
      sb.append(")")
    } else {
      docTypeTree(tpe)
    }
  }

  /**
    * Documents `tpe` as it should appear in the argument position of a function type: like
    * [[docDelimitedType]], except that a tuple gets an extra set of parentheses, to distinguish a
    * single tuple argument (e.g. `((Int32, Int32)) -> Int32`) from the uncurried multi-argument
    * sugar that reuses the same tuple-like syntax (e.g. `(Int32, Int32) -> Int32`).
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docFunctionArgType(tpe: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = tpe.baseType match {
    case Type.Cst(TypeConstructor.Tuple(_), _) =>
      sb.append("(")
      docTypeTree(tpe)
      sb.append(")")
    case _ => docDelimitedType(tpe)
  }

  /**
    * Documents the function type headed by `Arrow(arity)` applied to `args`, i.e. `[eff, arg_1,
    * ..., arg_n, ret]`, as `arg_1 -> ... -> arg_n -> ret \ eff` (with the `\ eff` omitted if
    * `eff` is `Pure`), linking and giving effect roles to whatever is nested in `arg_1..n` and
    * `ret`.
    *
    * `whole` is the original type, used only to fall back to [[docFallback]] if `args` is not of
    * the shape a fully-applied arrow should have (which should not happen for a well-kinded
    * type).
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docArrow(arity: Int, args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = args match {
    case eff :: rest if rest.length == arity && arity >= 2 =>
      val leading = rest.dropRight(2)
      val lastArg = rest(rest.length - 2)
      val ret = rest.last

      for (a <- leading) {
        docFunctionArgType(a)
        sb.append(" -> ")
      }
      docFunctionArgType(lastArg)
      sb.append(" -> ")
      docDelimitedType(ret)

      if (!isPureEffect(eff)) {
        sb.append(" \\ ")
        docEffectFormula(eff)
      }

    case _ => docFallback(whole)
  }

  /**
    * Documents the tuple type headed by `Tuple(arity)` applied to `args` as `(t_1, ..., t_n)`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTuple(arity: Int, args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (args.length != arity) {
      docFallback(whole)
      return
    }
    sb.append("(")
    docList(args)(docTypeTree)
    sb.append(")")
  }

  /**
    * Documents a predicate-like type headed by a constructor named `name` (`Relation`), applied
    * to `args`, as `name(t_1, ..., t_n)`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docParenWrapped(name: String, arity: Int, args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (args.length != arity) {
      docFallback(whole)
      return
    }
    docPlainTypeName(name)
    sb.append("(")
    docList(args)(docTypeTree)
    sb.append(")")
  }

  /**
    * Documents the lattice type headed by `Lattice(arity)` applied to `args` as
    * `Lattice(t_1, ..., t_n; lat)`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docLattice(arity: Int, args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    if (args.length != arity || args.isEmpty) {
      docFallback(whole)
      return
    }
    docPlainTypeName("Lattice")
    sb.append("(")
    docList(args.init)(docTypeTree)
    sb.append("; ")
    docTypeTree(args.last)
    sb.append(")")
  }

  /**
    * Documents the record type headed by `Record` applied to `args` (which should be a single
    * record row) as `{ label_1 = t_1, ..., label_n = t_n }`, or `{ ... | rest }` if the row is
    * extended by a variable.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docRecord(args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = args match {
    case rowTpe :: Nil =>
      val (labels0, rest) = collectRecordRow(rowTpe)
      val labels = labels0.sortBy(_._1)
      sb.append("{ ")
      docList(labels) { case (name, tpe) =>
        sb.append(esc(name))
        sb.append(" = ")
        docTypeTree(tpe)
      }
      rest.foreach { r =>
        if (labels.nonEmpty) sb.append(" | ")
        docTypeTree(r)
      }
      sb.append(" }")
    case _ => docFallback(whole)
  }

  /**
    * Decomposes the given record row, `row0`, into its labelled fields and, if the row is
    * extended by something other than the empty row, that remainder.
    *
    * Mirrors [[DisplayType.fromRecordRow]], but keeps each field's own [[Type]] rather than
    * converting to a [[DisplayType]], so the fields can still be linked and structured when
    * documented.
    */
  private def collectRecordRow(row0: Type): (List[(String, Type)], Option[Type]) = row0.baseType match {
    case Type.Cst(TypeConstructor.RecordRowEmpty, _) if row0.typeArguments.isEmpty => (Nil, None)
    case Type.Cst(TypeConstructor.RecordRowExtend(label), _) =>
      row0.typeArguments match {
        case tpe :: rest :: Nil =>
          val (labels, tail) = collectRecordRow(rest)
          ((label.name, tpe) :: labels, tail)
        case _ => (Nil, Some(row0))
      }
    case _ => (Nil, Some(row0))
  }

  /**
    * Documents the schema type headed by `Schema` applied to `args` (which should be a single
    * schema row) as `#{ Name_1(t_1, ...), ... }`, or `#{ ... | rest }` if the row is extended by
    * a variable.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSchema(args: List[Type], whole: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = args match {
    case rowTpe :: Nil =>
      val (fields0, rest) = collectSchemaRow(rowTpe)
      val fields = fields0.sortBy(_._1)
      sb.append("#{ ")
      docList(fields) { case (name, tpe) => docSchemaField(name, tpe) }
      rest.foreach { r =>
        if (fields.nonEmpty) sb.append(" | ")
        docTypeTree(r)
      }
      sb.append(" }")
    case _ => docFallback(whole)
  }

  /**
    * Decomposes the given schema row, `row0`, into its named fields and, if the row is extended
    * by something other than the empty row, that remainder.
    *
    * Mirrors [[DisplayType.fromSchemaRow]], but keeps each field's own [[Type]] rather than
    * converting to a [[DisplayType]].
    */
  private def collectSchemaRow(row0: Type): (List[(String, Type)], Option[Type]) = row0.baseType match {
    case Type.Cst(TypeConstructor.SchemaRowEmpty, _) if row0.typeArguments.isEmpty => (Nil, None)
    case Type.Cst(TypeConstructor.SchemaRowExtend(pred), _) =>
      row0.typeArguments match {
        case tpe :: rest :: Nil =>
          val (fields, tail) = collectSchemaRow(rest)
          ((pred.name, tpe) :: fields, tail)
        case _ => (Nil, Some(row0))
      }
    case _ => (Nil, Some(row0))
  }

  /**
    * Documents one field, `name(tpe)`, of a schema, printing it as a relation `name(t_1, ..., t_n)`
    * or a lattice `name(t_1, ..., t_n; lat)` if `tpe` (with any top-level alias erased) is one,
    * and as `name(<t>)` otherwise, mirroring [[DisplayType.visitSchemaFieldType]].
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docSchemaField(name: String, tpe: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {
    val erased = Type.eraseTopAliases(tpe)
    erased.baseType match {
      case Type.Cst(TypeConstructor.Relation(arity), _) if erased.typeArguments.length == arity =>
        sb.append(esc(name))
        sb.append("(")
        docList(erased.typeArguments)(docTypeTree)
        sb.append(")")
      case Type.Cst(TypeConstructor.Lattice(arity), _) if erased.typeArguments.length == arity && arity > 0 =>
        sb.append(esc(name))
        sb.append("(")
        docList(erased.typeArguments.init)(docTypeTree)
        sb.append("; ")
        docTypeTree(erased.typeArguments.last)
        sb.append(")")
      case _ =>
        sb.append(esc(name))
        sb.append("(<")
        docTypeTree(tpe)
        sb.append(">)")
    }
  }

  /**
    * Recursively documents the given `Type`, `eff0`, of kind `Eff`, one HTML element per effect
    * name, region and effect-kinded type variable, so that an effect nested anywhere in a type
    * -- e.g. inside a parameter's own function type -- gets the same `effect` role a top-level
    * effect gets.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffectFormula(eff0: Type)(implicit flix: Flix, documented: DocumentedSymbols, sb: StringBuilder): Unit = {

    /** Flattens a chain of the same effect operator `tc`, e.g. `(a + b) + c` into `a :: b :: c :: Nil`. */
    def flatten(tc: TypeConstructor, t: Type): List[Type] = t.baseType match {
      case Type.Cst(tc2, _) if tc2 == tc => t.typeArguments.flatMap(flatten(tc, _))
      case _ => List(t)
    }

    def needsEffectParens(t: Type): Boolean = t.baseType match {
      case Type.Cst(TypeConstructor.Complement | TypeConstructor.Union | TypeConstructor.Intersection
                    | TypeConstructor.Difference | TypeConstructor.SymmetricDiff, _) => true
      case _ => false
    }

    def visitDelimited(t: Type): Unit = {
      if (needsEffectParens(t)) {
        sb.append("(")
        visit(t)
        sb.append(")")
      } else {
        visit(t)
      }
    }

    def visitChain(op: TypeConstructor, sep: String, t: Type): Unit = {
      val parts = flatten(op, t)
      for ((p, i) <- parts.zipWithIndex) {
        visitDelimited(p)
        if (i < parts.length - 1) {
          sb.append(sep)
        }
      }
    }

    def visit(t: Type): Unit = t.baseType match {
      case Type.Cst(TypeConstructor.Effect(sym, _), _) =>
        docEffectSymName(sym)
        docTypeArgsInBrackets(t.typeArguments)

      case Type.Cst(TypeConstructor.Region(sym), _) =>
        sb.append(s"<span class='effect'>${esc(sym.text)}</span>")

      case Type.Var(sym, _) =>
        sb.append("<span class='effect'>")
        sb.append(esc(varName(sym)))
        sb.append("</span>")

      case Type.Cst(TypeConstructor.Pure, _) => sb.append("<span class='effect'>Pure</span>")
      case Type.Cst(TypeConstructor.Univ, _) => sb.append("<span class='effect'>Univ</span>")

      case Type.Cst(TypeConstructor.Complement, _) =>
        sb.append("~")
        visitDelimited(t.typeArguments.head)

      case Type.Cst(TypeConstructor.Union, _) => visitChain(TypeConstructor.Union, " + ", t)
      case Type.Cst(TypeConstructor.Intersection, _) => visitChain(TypeConstructor.Intersection, " & ", t)
      case Type.Cst(TypeConstructor.Difference, _) => visitChain(TypeConstructor.Difference, " - ", t)
      case Type.Cst(TypeConstructor.SymmetricDiff, _) => visitChain(TypeConstructor.SymmetricDiff, " ⊕ ", t)

      case Type.Alias(symUse, _, _, _) =>
        docAliasName(symUse.sym)
        docTypeArgsInBrackets(t.typeArguments)

      case _ => docFallback(t)
    }

    visit(eff0)
  }

  /**
    * Documents `t` using the shared formatter, [[FormatType]], on just this subtree, rather than
    * dropping it. Used for the handful of type or effect constructors that [[docTypeTree]] and
    * [[docEffectFormula]] do not walk individually.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docFallback(t: Type)(implicit flix: Flix, sb: StringBuilder): Unit = {
    val cls = if (t.kind == Kind.Eff) "effect" else "type"
    sb.append(s"<span class='$cls'>")
    sb.append(esc(FormatType.formatType(t)))
    sb.append("</span>")
  }

  /**
    * Runs the given `docElt` on each element of `list`, separated by the string: ", " (comma + space)
    */
  private def docList[T](list: List[T])(docElt: T => Unit)(implicit sb: StringBuilder): Unit = {
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
  private def writeAssets(outputDir: Path): Unit = {
    val stylesheet = readResourceString(Stylesheet) + mkIconStyles()
    writeFile("styles.css", stylesheet.getBytes, outputDir)

    val favicon = readResource(FavIcon)
    writeFile("favicon.png", favicon, outputDir)

    val script = readResource(Script)
    writeFile("index.js", script, outputDir)
  }

  /**
    * Append the icon with the given CSS class, `cls`, to the given `StringBuilder`.
    *
    * The element is empty: its artwork is applied by the stylesheet, see [[mkIconStyles]].
    */
  private def docIcon(cls: String)(implicit sb: StringBuilder): Unit = {
    sb.append(s"<span class='$cls icon'></span>")
  }

  /**
    * Returns the CSS rules that give each icon of [[IconClasses]] its artwork, as a mask.
    *
    * The icons are put in the stylesheet, rather than inlined into the HTML, since each page would
    * otherwise carry a copy of every icon it uses. Masking, rather than embedding the SVG as an
    * image, keeps the icons able to inherit the `color` of their parent.
    *
    * The attribution comments of each icon are stripped, since they carry no meaning in the output.
    * They are kept in the SVG files themselves, along with `icons/LICENSE.md`.
    */
  private def mkIconStyles(): String = {
    val rules = IconClasses.map {
      case (cls, name) =>
        val svg = readResourceString(s"$Icons/$name.svg")
        val stripped = CommentPattern.matcher(svg).replaceAll("").trim
        s".$cls.icon { --icon: url(\"data:image/svg+xml,${escDataUri(stripped)}\") }"
    }
    rules.mkString("\n", "\n", "\n")
  }

  /**
    * Write the documentation output string into the output directory with the given `name`.
    */
  private def writeDocFile(name: String, output: String, outputDir: Path): Unit = {
    writeFile(s"$name", output.getBytes, outputDir)
  }

  /**
    * Write the file to the output directory with the given file name.
    */
  private def writeFile(name: String, output: Array[Byte], outputDir: Path): Unit = {
    val path = outputDir.resolve(name)
    try {
      Files.createDirectories(outputDir)
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
    * Create a raw link to the given `SourceLocation`, if it has one.
    *
    * The link is to the lines of `loc` on the page of its source, see [[HtmlHighlighter]].
    *
    * The URL is already escaped.
    */
  private def createLink(loc: SourceLocation): Option[String] = HtmlHighlighter.link(loc)

  /**
    * Escape any HTML in the string.
    */
  private def esc(s: String): String = xml.Utility.escape(s)

  /**
    * Transform the string into a valid URL.
    */
  private def escUrl(s: String): String = URLEncoder.encode(s, "UTF-8")

  /**
    * Escape the string for inclusion in a `data:` URI.
    *
    * Unlike in a query string, a `+` denotes itself in a `data:` URI, so spaces must be escaped.
    */
  private def escDataUri(s: String): String = escUrl(s).replace("+", "%20")

  /**
    * An item is a unit that is typically output to its own HTML file.
    */
  private sealed trait Item {
    /** The shortest name of the item, e.g. 'StdOut' */
    def name: String

    /** The fully qualified name of the item, e.g. 'System.StdOut' */
    def qualifiedName: String

    /** The file name of the item, e.g. 'System.StdOut.html' */
    def fileName: String
  }

  /**
    * A representation of a module that's easier to work with while generating documentation.
    */
  private case class Module(sym: Symbol.ModuleSym,
                            doc: Doc,
                            parent: Option[Symbol.ModuleSym],
                            uses: List[UseOrImport],
                            submodules: List[Module],
                            traits: List[Trait],
                            effects: List[Effect],
                            enums: List[Enum],
                            structs: List[Struct],
                            typeAliases: List[TypedAst.TypeAlias],
                            defs: List[TypedAst.Def]) extends Item {
    override def name: String = moduleName(this.sym)

    override def qualifiedName: String = moduleQualifiedName(this.sym)

    override def fileName: String = moduleFileName(this.sym)
  }

  /**
    * A representation of a trait that's easier to work with while generating documentation.
    */
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

  /**
    * A representation of an effect that's easier to work with while generating documentation.
    */
  private case class Effect(decl: TypedAst.Effect,
                            defaultHandler: Option[Symbol.DefnSym],
                            parent: Symbol.ModuleSym,
                            companionMod: Option[Module]) extends Item {
    override def name: String = effectName(this.decl.sym)

    override def qualifiedName: String = effectQualifiedName(this.decl.sym)

    override def fileName: String = effectFileName(this.decl.sym)
  }

  /**
    * A representation of an enum that's easier to work with while generating documentation.
    */
  private case class Enum(decl: TypedAst.Enum,
                          instances: List[TypedAst.Instance],
                          parent: Symbol.ModuleSym,
                          companionMod: Option[Module]) extends Item {
    override def name: String = enumName(this.decl.sym)

    override def qualifiedName: String = enumQualifiedName(this.decl.sym)

    override def fileName: String = enumFileName(this.decl.sym)
  }

  /**
    * A representation of a struct that's easier to work with while generating documentation.
    */
  private case class Struct(decl: TypedAst.Struct,
                            instances: List[TypedAst.Instance],
                            parent: Symbol.ModuleSym,
                            companionMod: Option[Module]) extends Item {
    override def name: String = structName(this.decl.sym)

    override def qualifiedName: String = structQualifiedName(this.decl.sym)

    override def fileName: String = structFileName(this.decl.sym)
  }
}
