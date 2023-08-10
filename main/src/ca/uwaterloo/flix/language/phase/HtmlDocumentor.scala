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

  def run(root: TypedAst.Root)(implicit flix: Flix): TypedAst.Root = flix.phase("HtmlDocumentor") {
    //
    // Determine whether to generate documentation.
    //
    if (!flix.options.documentorHtml) {
      return root
    }

    // TODO clear directory?
    val modules = splitModules(root)
    modules.par.foreach {
      mod =>
        val pub = filterPublic(mod)
        if (!isEmpty(pub)) {
          val out = documentModule(pub)
          writeFile(mod, out)
        }
    }

    root
  }

  private def splitModules(root: TypedAst.Root): Iterable[Module] = root.modules.map {
    case (sym, mod) =>
      val namespace = sym.ns
      val uses = root.uses.getOrElse(sym, Nil)

      var submodules: List[Symbol.ModuleSym] = Nil
      var classes: List[TypedAst.Class] = Nil
      var enums: List[TypedAst.Enum] = Nil
      var restrictableEnums: List[TypedAst.RestrictableEnum] = Nil
      var effects: List[TypedAst.Effect] = Nil
      var typeAliases: List[TypedAst.TypeAlias] = Nil
      var defs: List[TypedAst.Def] = Nil
      mod.foreach {
        case sym: Symbol.ModuleSym => submodules = sym :: submodules
        case sym: Symbol.ClassSym => classes = root.classes(sym) :: classes
        case sym: Symbol.EnumSym => enums = root.enums(sym) :: enums
        case sym: Symbol.RestrictableEnumSym => restrictableEnums = root.restrictableEnums(sym) :: restrictableEnums
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
        restrictableEnums,
        effects,
        typeAliases,
        defs,
      )
  }

  private def filterPublic(mod: Module): Module = mod match {
    case Module(namespace, uses, submodules, classes, enums, restrictableEnums, effects, typeAliases, defs) =>
      Module(
        namespace,
        uses,
        submodules,
        classes.filter(c => c.mod.isPublic && c.ann.isInternal),
        enums.filter(e => e.mod.isPublic && !e.ann.isInternal),
        restrictableEnums.filter(e => e.mod.isPublic && !e.ann.isInternal),
        effects.filter(e => e.mod.isPublic && !e.ann.isInternal),
        typeAliases.filter(t => t.mod.isPublic),
        defs.filter(d => d.spec.mod.isPublic && !d.spec.ann.isInternal),
      )
  }

  private def isEmpty(mod: HtmlDocumentor.Module): Boolean = mod match {
    case Module(_, _, _, classes, enums, restrictableEnums, effects, typeAliases, defs) =>
      classes.isEmpty && enums.isEmpty && restrictableEnums.isEmpty && effects.isEmpty && typeAliases.isEmpty && defs.isEmpty
  }

  private def documentModule(mod: Module)(implicit flix: Flix): String = {
    implicit val sb: StringBuilder = new StringBuilder()

    val name = if (mod.namespace.isEmpty) RootNS else mod.namespace.mkString(".")

    sb.append(mkHead(name))
    sb.append("<body>")

    sb.append("<h1>")
    sb.append(name)
    sb.append("</h1><hr>")

    docEnums(mod.enums)
    docTypeAliases(mod.typeAliases)
    docDefs(mod.defs)

    sb.append("</body>")

    sb.toString()
  }

  private def mkHead(name: String): String =
    "<!doctype html><html lang='en'>" +
      "<head>" +
      "<meta charset='utf-8'/>" +
      "<meta name='viewport' content='width=device-width,initial-scale=1'/>" +
      // TODO remake stylesheet
      "<link href='https://fonts.googleapis.com/css?family=Fira+Code&display=swap' rel='stylesheet'>" +
      "<link href='https://fonts.googleapis.com/css?family=Oswald&display=swap' rel='stylesheet'>" +
      "<link href='https://api.flix.dev/static/css/main.019098b1.css' rel='stylesheet'>" +
      s"<title>Flix Doc | $name</title>" +
      "</head>"

  private def docEnums(enums: List[TypedAst.Enum])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (enums.isEmpty) {
      return
    }

    sb.append("<div><h2>Enums</h2>")

    for (e <- enums.sortBy(_.sym.name)) {
      sb.append("<div class='box'><div>")

      sb.append("<span class='line'><span class='keyword'>enum</span> ")
      sb.append(s"<span class='name'>${e.sym.name}</span>")
      docTypeParams(e.tparams, showKinds = true)
      docDerivations(e.derives)

      docSourceLocation(e.loc)

      sb.append("</div>")

      docCases(e.cases.values.toList)

      docDoc(e.doc)

      sb.append("</div>")
    }

    sb.append("</div>")
  }

  private def docTypeAliases(typeAliases: List[TypedAst.TypeAlias])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (typeAliases.isEmpty) {
      return
    }

    sb.append("<div><h2>Type Aliases</h2>")

    for (t <- typeAliases.sortBy(_.sym.name)) {
      sb.append("<div class='box'><div>")

      sb.append("<span class='line'><span class='keyword'>type alias</span> ")
      sb.append(s"<span class='name'>${t.sym.name}</span>")
      docTypeParams(t.tparams, showKinds = true)
      sb.append(" = ")
      docType(t.tpe)

      docSourceLocation(t.loc)

      sb.append("</div>")

      docDoc(t.doc)

      sb.append("</div>")
    }

    sb.append("</div>")
  }

  private def docDefs(defs: List[TypedAst.Def])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (defs.isEmpty) {
      return
    }

    sb.append("<div><h2>Definitions</h2>")

    for (d <- defs.sortBy(_.sym.text)) {
      sb.append("<div class='box'><div>")

      sb.append("<span class='line'><span class='keyword'>def</span> ")
      sb.append(s"<span class='name'>${d.sym.name}</span>")
      docTypeParams(d.spec.tparams, showKinds = false)
      docFormalParams(d.spec.fparams)
      sb.append("<span>: ")
      docType(d.spec.retTpe)
      sb.append(" \\ <span>")
      docType(d.spec.eff)
      sb.append("</span></span></span>")

      docSourceLocation(d.spec.loc)

      sb.append("</div>")

      docDoc(d.spec.doc)

      sb.append("</div>")
    }

    sb.append("</div>")
  }

  private def docDerivations(derives: Ast.Derivations)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (derives.classes.isEmpty) {
      return
    }

    sb.append("<span> <span class='keyword'>with</span> ")

    for ((c, i) <- derives.classes.sortBy(_.loc).zipWithIndex) {
      sb.append("<span class='tpe-constraint'>")
      sb.append(c.clazz.name)
      sb.append("</span>")

      if (i < derives.classes.length - 1) {
        sb.append(", ")
      }
    }

    sb.append("</span>")
  }

  private def docCases(cases: List[TypedAst.Case])(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<div>")

    for (c <- cases.sortBy(_.loc)) {
      sb.append("<div><span class='keyword'>case</span> <span>")
      sb.append(c.sym.name)
      sb.append("</span>(")

      SimpleType.fromWellKindedType(c.tpe)(flix.getFormatOptions) match {
        case SimpleType.Tuple(fields) =>
          for ((t, i) <- fields.zipWithIndex) {
            sb.append("<span class='type'>")
            sb.append(FormatType.formatSimpleType(t))
            sb.append("</span>")

            if (i < fields.length - 1) {
              sb.append(", ")
            }
          }
        case _ => docType(c.tpe)
      }

      sb.append(")</div>")
    }

    sb.append("</div>")
  }

  private def docTypeParams(tparams: List[TypedAst.TypeParam], showKinds: Boolean)(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (tparams.isEmpty) {
      return
    }

    sb.append("<span class='tparams'>[")
    for ((p, i) <- tparams.sortBy(_.loc).zipWithIndex) {
      sb.append("<span class='tparam'><span class='type'>")
      sb.append(p.name)
      sb.append("</span>")
      if (showKinds) {
        sb.append(": <span class='kind'>")
        sb.append(p.sym.kind)
        sb.append("</span>")
      }
      sb.append("</span>")

      if (i < tparams.length - 1) {
        sb.append(", ")
      }
    }
    sb.append("]</span>")
  }

  private def docFormalParams(fparams: List[TypedAst.FormalParam])(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='fparams'>(")
    for ((p, i) <- fparams.sortBy(_.loc).zipWithIndex) {
      sb.append("<span><span>")
      sb.append(p.sym.text)
      sb.append("</span>: ")
      docType(p.tpe)
      sb.append("</span>")

      if (i < fparams.length - 1) {
        sb.append(", ")
      }
    }
    sb.append(")</span>")
  }

  private def docSourceLocation(loc: SourceLocation)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append(s"<span class='source'><a target='_blank' href='${createLink(loc)}'>Source</a></span>")
  }

  private def docDoc(doc: Ast.Doc)(implicit flix: Flix, sb: StringBuilder): Unit = {
    // DEFAULT_SAFE escapes HTML
    val config = txtmark.Configuration.DEFAULT_SAFE
    val parsed = txtmark.Processor.process(doc.text, config);

    sb.append("<div class='doc'>")
    sb.append(parsed)
    sb.append("</div>")
  }

  private def docType(tpe: Type)(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='type'>")
    sb.append(FormatType.formatType(tpe))
    sb.append("</span>")
  }

  private def writeFile(mod: Module, output: String): Unit = {
    val name = if (mod.namespace.isEmpty) List(RootNS) else mod.namespace
    val path = OutputDirectory.resolve(s"${name.mkString(".")}.html")

    try {
      Files.createDirectories(OutputDirectory)
      val writer = Files.newBufferedWriter(path)
      writer.write(output)
      writer.close()
    } catch {
      case ex: IOException => throw new RuntimeException(s"Unable to write to path '$path'.", ex)
    }
  }

  private def createLink(loc: SourceLocation): String = {
    // TODO make it also work for local user code
    s"https://github.com/flix/flix/blob/master/main/src/library/${loc.source.name}#L${loc.beginLine}-L${loc.beginLine}"
  }

  private case class Module(namespace: List[String],
                            uses: List[Ast.UseOrImport],
                            submodules: List[Symbol.ModuleSym],
                            classes: List[TypedAst.Class],
                            enums: List[TypedAst.Enum],
                            restrictableEnums: List[TypedAst.RestrictableEnum],
                            effects: List[TypedAst.Effect],
                            typeAliases: List[TypedAst.TypeAlias],
                            defs: List[TypedAst.Def])
}
