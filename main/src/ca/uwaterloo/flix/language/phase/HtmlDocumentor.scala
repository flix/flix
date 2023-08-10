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
import ca.uwaterloo.flix.language.fmt.FormatType

import java.io.IOException
import java.nio.file.{Files, Path, Paths}
import scala.collection.parallel.CollectionConverters.IterableIsParallelizable

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
        classes.filter(_.mod.isPublic),
        enums.filter(_.mod.isPublic),
        restrictableEnums.filter(_.mod.isPublic),
        effects.filter(_.mod.isPublic),
        typeAliases.filter(_.mod.isPublic),
        defs.filter(_.spec.mod.isPublic),
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

  private def docTypeAliases(typeAliases: List[TypedAst.TypeAlias])(implicit flix: Flix, sb: StringBuilder): Unit = {
    if (typeAliases.isEmpty) {
      return
    }

    sb.append("<div><h2>Type Aliases</h2>")

    for (t <- typeAliases.sortBy(_.sym.name)) {
      sb.append("<div class='box'><div>")

      sb.append("<span class='line'><span class='keyword'>type alias</span> ")
      sb.append(s"<span class='name'>${t.sym.name}</span> = ")
      docType(t.tpe)

      sb.append(s"<span class='source'><a target='_blank' href='${createLink(t.loc)}'>Source</a></span>")

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
      docTypeParams(d.spec.tparams)
      docFormalParams(d.spec.fparams)
      sb.append("<span>: ")
      docType(d.spec.retTpe)
      sb.append(" \\ <span>")
      docType(d.spec.eff)
      sb.append("</span></span></span>")

      sb.append(s"<span class='source'><a target='_blank' href='${createLink(d.spec.loc)}'>Source</a></span>")

      sb.append("</div>")

      docDoc(d.spec.doc)

      sb.append("</div>")
    }

    sb.append("</div>")
  }

  private def docTypeParams(tparams: List[TypedAst.TypeParam])(implicit flix: Flix, sb: StringBuilder): Unit = {
    sb.append("<span class='tparams'>[")
    for ((p, i) <- tparams.sortBy(_.loc).zipWithIndex) {
      sb.append("<span class='tparam'><span class='type'>")
      sb.append(p.name)
      sb.append("</span></span>")

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

  private def docDoc(doc: Ast.Doc)(implicit sb: StringBuilder): Unit = {
    val escaped = xml.Utility.escape(doc.text)
    // TODO parse markdown
    val parsed = escaped

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
