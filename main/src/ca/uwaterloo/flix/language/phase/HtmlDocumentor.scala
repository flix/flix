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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.fmt.{FormatType, SimpleType}

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable
import com.github.rjeschke.txtmark
import scala.io.Source

/**
  * A phase that emits a JSON file for library documentation.
  */
object HtmlDocumentor {

  /**
    * The "Pseudo-name" of the root namespace.
    */
  val RootNS: String = "Prelude"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./build/doc")

  /**
    * The path to the the stylesheet, relative to the resources folder.
    */
  val Stylesheet: String = "/doc/styles.css"

  def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = flix.phase("HtmlDocumentor") {
    //
    // Determine whether to generate documentation.
    //
    if (!flix.options.documentorHtml) {
      return root
    }

    clearOutputDirectory()
    writeStyles()
    val modules = splitModules(root)
    modules.par.foreach {
      mod =>
        val pub = filterModule(mod)
        if (!isEmpty(pub)) {
          val out = documentModule(pub)
          writeModule(mod, out)
        }
    }

    root
  }

  /**
    * Splits the modules present in the root into a set of `HtmlDocumentor.Module`s, making them easier to work with.
    */
  private def splitModules(root: TypedAst.Root): Iterable[Module] = root.modules.map {
    case (sym, mod) =>
      val namespace = sym.ns
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

      Module(
        namespace,
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
    * Returns a `Module` corresponding to the given `mod`,
    * but with all items that shouldn't appear in the documentation removed.
    */
  private def filterModule(mod: Module): Module = mod match {
    case Module(namespace, uses, submodules, classes, enums, effects, typeAliases, defs) =>
      Module(
        namespace,
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
    * Checks whether the given `Module`, `mod`, contains anything or not.
    */
  private def isEmpty(mod: HtmlDocumentor.Module): Boolean = mod match {
    case Module(_, _, _, classes, enums, effects, typeAliases, defs) =>
      classes.isEmpty && enums.isEmpty && effects.isEmpty && typeAliases.isEmpty && defs.isEmpty
  }

  /**
    * Documents the given `Module`, `mod`, returning a string of HTML.
    */
  private def documentModule(mod: Module)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val name = if (mod.namespace.isEmpty) RootNS else mod.namespace.mkString(".")

    sb.append(mkHead(name))
    sb.append("<body>")

    sb.append(s"<h1>${esc(name)}</h1>")
    sb.append("<hr/>")

    docSection("Classes", mod.classes.sortBy(_.sym.name), docClass)
    docSection("Enums", mod.enums.sortBy(_.sym.name), docEnum)
    docSection("Effects", mod.effects.sortBy(_.sym.name), docEffect)
    docSection("Type Aliases", mod.typeAliases.sortBy(_.sym.name), docTypeAlias)
    docSection("Definitions", mod.defs.sortBy(_.sym.name), docDef)

    sb.append("</body>")

    sb.toString()
  }

  /**
    * Generates the string representing the head of the HTML document.
    */
  private def mkHead(name: String): String = {
    "<!doctype html><html lang='en'>" +
      "<head>" +
      "<meta charset='utf-8'/>" +
      "<meta name='viewport' content='width=device-width,initial-scale=1'/>" +
      "<link href='https://fonts.googleapis.com/css?family=Fira+Code&display=swap' rel='stylesheet'>" +
      "<link href='https://fonts.googleapis.com/css?family=Oswald&display=swap' rel='stylesheet'>" +
      "<link href='styles.css' rel='stylesheet'>" +
      s"<title>Flix Doc | ${esc(name)}</title>" +
      "</head>"
  }

  /**
    * Documents a section, (Classes, Enums, Effects, etc.), containing a `group` of items, each being placed in a box.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    *
    * If `group` is empty, nothing will be generated.
    *
    * @param name   The name of the section, e.g. "Classes".
    * @param group  The list of items in the section, in the order that they should appear.
    * @param docElt A function taking a single item from `group` and generating the corresponding HTML string.
    *               Note that this will be already be wrapped in a box.
    */
  private def docSection[T](name: String, group: List[T], docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append("<section>")
    sb.append(s"<h2>${esc(name)}</h2>")
    for (e <- group) {
      sb.append("<div class='box'>")
      docElt(e)
      sb.append("</div>")
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
    */
  private def docSubSection[T](name: String, group: List[T], docElt: T => Unit)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (group.isEmpty) {
      return
    }

    sb.append("<details>")
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
    docAnnotations(clazz.ann)
    sb.append("<code>")
    sb.append("<span class='keyword'>class</span> ")
    sb.append(s"<span class='name'>${esc(clazz.sym.name)}</span>")
    docTypeParams(List(clazz.tparam), showKinds = true)
    docTypeConstraints(clazz.superClasses)
    sb.append("</code>")
    docSourceLocation(clazz.loc)
    docDoc(clazz.doc)
    docSubSection("Signatures", clazz.signatures.sortBy(_.sym.name), docSignature)
    docSubSection("Definitions", clazz.defs.sortBy(_.sym.name), docSignature)
    docSubSection("Instances", clazz.instances.sortBy(_.loc), docInstance)
  }

  /**
    * Documents the given `Enum`, `enm`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEnum(enm: TypedAst.Enum)(implicit flix: Flix, sb: StringBuilder): Unit = {
    docAnnotations(enm.ann)
    sb.append("<code>")
    sb.append("<span class='keyword'>enum</span> ")
    sb.append(s"<span class='name'>${esc(enm.sym.name)}</span>")
    docTypeParams(enm.tparams, showKinds = true)
    docDerivations(enm.derives)
    sb.append("</code>")
    docSourceLocation(enm.loc)
    docCases(enm.cases.values.toList)
    docDoc(enm.doc)
  }

  /**
    * Documents the given `Effect`, `eff`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffect(eff: TypedAst.Effect)(implicit flix: Flix, sb: StringBuilder): Unit = {
    docAnnotations(eff.ann)
    sb.append("<code>")
    sb.append("<span class='keyword'>eff</span> ")
    sb.append(s"<span class='name'>${esc(eff.sym.name)}</span>")
    sb.append("</code>")
    docSourceLocation(eff.loc)
    docSubSection("Ops", eff.ops, (o: TypedAst.Op) => docSpec(o.sym.name, o.spec))
    docDoc(eff.doc)
  }

  /**
    * Documents the given `TypeAlias`, `ta`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docTypeAlias(ta: TypedAst.TypeAlias)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<code>")
    sb.append("<span class='keyword'>type alias</span> ")
    sb.append(s"<span class='name'>${esc(ta.sym.name)}</span>")
    docTypeParams(ta.tparams, showKinds = true)
    sb.append(" = ")
    docType(ta.tpe)
    sb.append("</code>")
    docSourceLocation(ta.loc)
    docDoc(ta.doc)
  }

  /**
    * Documents the given `Def`, `defn`.
    *
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docDef(defn: TypedAst.Def)(implicit flix: Flix, sb: StringBuilder): Unit =
    docSpec(defn.sym.name, defn.spec)

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
    sb.append("<code>")
    sb.append("<span class='keyword'>def</span> ")
    sb.append(s"<span class='name'>${esc(name)}</span>")
    docTypeParams(spec.tparams, showKinds = false)
    docFormalParams(spec.fparams)
    sb.append(": ")
    docType(spec.retTpe)
    sb.append(" \\ ")
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
    *
    * @param showKinds  Whether or not the kinds of the types should be included.
    *                   Example: {{{
    *                    docTypeParams(... showKinds = false) -> "[a, b, ef]"
    *                    docTypeParams(... showKinds = true) -> "[a: Type, b: Type -> Type, ef: Eff]"
    *                   }}}
    */
  private def docTypeParams(tparams: List[TypedAst.TypeParam], showKinds: Boolean)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (tparams.isEmpty) {
      return
    }

    sb.append("<span class='tparams'>[")
    docList(tparams.sortBy(_.loc)) { p =>
      sb.append("<span class='tparam'>")
      sb.append(s"<span class='type'>${esc(p.name.name)}</span>")
      if (showKinds) {
        sb.append(s": <span class='kind'>${esc(p.sym.kind.toString)}</span>")
      }
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
    for (a <- anns.annotations) {
      sb.append(s"<code class='annotation'>${esc(a.toString)}</code>")
    }
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
    // DEFAULT_SAFE escapes HTML
    val config = txtmark.Configuration.DEFAULT_SAFE
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
    * The result will be appended to the given `StringBuilder`, `sb`.
    */
  private def docEffectType(eff: Type)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='effect'>")
    sb.append(esc(FormatType.formatType(eff)))
    sb.append("</span>")
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

  private def clearOutputDirectory(): Unit = {
    Files.list(OutputDirectory).forEach {
      f => Files.delete(f)
    }
  }

  /**
    * Make a copy of the stylesheet into the output directory.
    */
  private def writeStyles(): Unit = {
    val source = Source.fromURL(getClass.getResource(Stylesheet))
    writeFile("styles.css", source.mkString)
  }

  /**
    * Write the documentation output string of the `Module`, `mod`, into the output directory with a suitable name.
    */
  private def writeModule(mod: Module, output: String): Unit = {
    val name = if (mod.namespace.isEmpty) List(RootNS) else mod.namespace
    writeFile(s"${name.mkString(".")}.html", output)
  }

  /**
    * Write the file to the output directory with the given file name.
    */
  private def writeFile(name: String, output: String): Unit = {
    val path = OutputDirectory.resolve(name)
    try {
      Files.createDirectories(OutputDirectory)
      Files.writeString(path, output)
    } catch {
      case ex: IOException => throw new RuntimeException(s"Unable to write to path '$path'.", ex)
    }
  }

  /**
    * Create a raw link to the given `SourceLocation`.
    */
  private def createLink(loc: SourceLocation): String = {
    // TODO make it also work for local user code
    s"https://github.com/flix/flix/blob/master/main/src/library/${loc.source.name}#L${loc.beginLine}-L${loc.beginLine}"
  }

  /**
    * Escape any HTML in the string.
    */
  private def esc(s: String): String = xml.Utility.escape(s)

  /**
    * A represention of a module that's easier to work with while generating documention.
    */
  private case class Module(namespace: List[String],
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
