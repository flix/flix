/*
 * Copyright 2016 Magnus Madsen
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

import java.io.IOException
import java.nio.file.{Files, Path, Paths}

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.language.ast.{Type, TypedAst}
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.tc.Show._
import ca.uwaterloo.flix.util.{LocalResource, StreamOps, Validation}
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

object Documentor extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * The directory where to write the generated HTML documentation (and its resources).
    */
  val OutputDirectory: Path = Paths.get("./target/api")

  /**
    * Generates documentation for the given program `p`.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Documentor") {
    // Check whether to generate documentation.
    if (flix.options.documentor) {
      // Collect the definitions.
      val defsByNS = root.defs.filterNot {
        case (sym, defn) => defn.ann.isLaw || defn.ann.isTest || !defn.mod.isPublic
      }.groupBy(_._1.namespace)

      // Collect the effects.
      val effsByNS = root.effs.filterNot {
        case (sym, defn) => !defn.mod.isPublic
      }.groupBy(_._1.namespace)

      // Collect the laws.
      val lawsByNS = root.defs.filter {
        case (sym, defn) => defn.ann.isLaw
      }.groupBy(_._1.namespace)

      // Collect the tests.
      val testsByNS = root.defs.filter {
        case (sym, defn) => defn.ann.isTest
      }.groupBy(_._1.namespace)

      // Collect the enums.
      val enumsByNS = root.enums.groupBy(_._1.namespace)

      // Collect the tables.
      val tablesByNS = root.tables.groupBy(_._1.namespace)

      // Collect the relations.
      val relationsByNS = tablesByNS.map {
        case (ns, m) => ns -> m.collect {
          case (sym, t: Table.Relation) => t
        }.toList
      }
      // Collect the lattices.
      val latticesByNS = tablesByNS.map {
        case (ns, m) => ns -> m.collect {
          case (sym, t: Table.Lattice) => t
        }.toList
      }

      // Compute the set of all available namespaces.
      val namespaces = defsByNS.keySet ++ effsByNS.keySet ++ lawsByNS.keySet ++ testsByNS.keySet ++ enumsByNS.keySet ++ tablesByNS.keySet

      // Process each namespace.
      val data = namespaces map {
        case ns =>
          val defs = defsByNS.getOrElse(ns, Nil).toList.map(kv => mkDefn(kv._2))
          val laws = lawsByNS.getOrElse(ns, Nil).toList.map(kv => mkDefn(kv._2))
          val effs = effsByNS.getOrElse(ns, Nil).toList.map(kv => mkEff(kv._2))
          val tests = testsByNS.getOrElse(ns, Nil).toList.map(kv => mkDefn(kv._2))
          val enums = enumsByNS.getOrElse(ns, Nil).toList.map(kv => mkEnum(kv._2))
          val relations = relationsByNS.getOrElse(ns, Nil) map mkRelation
          val lattices = latticesByNS.getOrElse(ns, Nil) map mkLattice

          ns -> JObject(
            JField("namespace", JString(ns.mkString("."))),
            JField("types", JArray(enums)),
            JField("definitions", JArray(defs)),
            JField("effs", JArray(effs)),
            JField("laws", JArray(laws)),
            JField("tests", JArray(tests)),
            JField("relations", JArray(relations)),
            JField("lattices", JArray(lattices))
          )
      }

      // Create the output directory (and its parent directories).
      Files.createDirectories(OutputDirectory)

      // Copy the JavaScript resource.
      val javaScriptPath = OutputDirectory.resolve("__app__.js")
      StreamOps.writeAll(LocalResource.Documentation.JavaScript, javaScriptPath)

      // Copy the StyleSheet resource.
      val styleSheetPath = OutputDirectory.resolve("__app__.css")
      StreamOps.writeAll(LocalResource.Documentation.StyleSheet, styleSheetPath)

      // Generate JSON for the menu.
      val menu = JArray(mkMenu(namespaces.filter(_.nonEmpty)))

      // Generate HTML files for each namespace.
      for ((ns, page) <- data) {
        writeString(mkHtmlPage(ns, menu, page), getHtmlPath(ns))
      }
    }

    root.toSuccess
  }

  /**
    * Returns a JSON object of the available namespaces for the menu.
    */
  private def mkMenu(xs: Set[List[String]]): List[JObject] = {
    val prelude = JObject(List(
      JField("name", JString("Prelude")),
      JField("link", JString("index.html"))
    ))

    val namespaces = xs map {
      case ns => JObject(List(
        JField("name", JString(ns.mkString("."))),
        JField("link", JString(ns.mkString(".") + ".html"))
      ))
    }

    prelude :: namespaces.toList
  }

  /**
    * Returns the given definition `d` as a JSON object.
    */
  private def mkDefn(d: TypedAst.Def): JObject = {
    // Process type parameters.
    val tparams = d.tparams.map {
      case TypeParam(ident, tpe, loc) => JObject(List(
        JField("name", JString(ident.name))
      ))
    }

    // Process formal parameters.
    val fparams = d.fparams.map {
      case FormalParam(psym, mod, tpe, loc) => JObject(
        JField("name", JString(psym.text)),
        JField("tpe", JString(prettify(tpe)))
      )
    }

    // Compute return type.
    val returnType = prettify(d.tpe.typeArguments.last)

    JObject(List(
      JField("name", JString(d.sym.name)),
      JField("tparams", JArray(tparams)),
      JField("fparams", JArray(fparams)),
      JField("result", JString(returnType)),
      JField("comment", JString(d.doc.text))
    ))

  }

  /**
    * Returns the given effect `d` as a JSON object.
    */
  private def mkEff(eff: TypedAst.Eff): JObject = {
    // Process type parameters.
    val tparams = eff.tparams.map {
      case TypeParam(ident, tpe, loc) => JObject(List(
        JField("name", JString(ident.name))
      ))
    }

    // Process formal parameters.
    val fparams = eff.fparams.map {
      case FormalParam(psym, mod, tpe, loc) => JObject(
        JField("name", JString(psym.text)),
        JField("tpe", JString(prettify(tpe)))
      )
    }

    // Compute return type.
    val returnType = prettify(eff.tpe.typeArguments.last)

    JObject(List(
      JField("name", JString(eff.sym.name)),
      JField("tparams", JArray(tparams)),
      JField("fparams", JArray(fparams)),
      JField("result", JString(returnType)),
      JField("comment", JString(eff.doc.text))
    ))

  }

  /**
    * Returns the given enum `e` as a JSON object.
    */
  private def mkEnum(e: TypedAst.Enum): JObject = {
    JObject(List(
      JField("name", JString(e.sym.name)),
      JField("comment", JString(e.doc.text))
    ))
  }

  /**
    * Returns the given relation `r` as a JSON object.
    */
  private def mkRelation(r: Table.Relation): JObject = {
    val attributes = r.attributes.map {
      case Attribute(name, tpe, loc) => JObject(List(
        JField("name", JString(name)),
        JField("tpe", JString(prettify(tpe)))
      ))
    }

    JObject(List(
      JField("name", JString(r.sym.name)),
      JField("attributes", JArray(attributes)),
      JField("comment", JString(r.doc.text))
    ))
  }

  /**
    * Returns the given lattice `l` as a JSON object.
    */
  private def mkLattice(l: Table.Lattice): JObject = {
    val attributes = (l.keys ::: l.value :: Nil).map {
      case Attribute(name, tpe, loc) => JObject(List(
        JField("name", JString(name)),
        JField("tpe", JString(prettify(tpe)))
      ))
    }

    JObject(List(
      JField("name", JString(l.sym.name)),
      JField("attributes", JArray(attributes)),
      JField("comment", JString(l.doc.text))
    ))
  }

  /**
    * Returns the path where the HTML file, for the given namespace, should be stored.
    */
  private def getHtmlPath(ns: List[String]): Path = {
    if (ns.isEmpty)
      OutputDirectory.resolve("index.html")
    else
      OutputDirectory.resolve(ns.mkString(".") + ".html")
  }

  /**
    * Writes the given string `s` to the given path `p`.
    */
  private def writeString(s: String, p: Path): Unit = try {
    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  } catch {
    case ex: IOException => throw new RuntimeException(s"Unable to write to path '$p'.", ex)
  }

  /**
    * Converts the given type into a pretty string.
    */
  private def prettify(t: Type): String = t.show

  /**
    * Returns the HTML fragment to use for the given namespace `ns`.
    */
  private def mkHtmlPage(ns: List[String], menu: JValue, page: JValue): String = {
    // Compute the page title (if any).
    val title = if (ns.isEmpty)
      "Flix Standard Library"
    else
      "Flix Standard Library: " + ns.mkString(".")

    // Render the menu JSON data into a string.
    val menuStr = JsonMethods.pretty(JsonMethods.render(menu))

    // Render the page JSON data into a string.
    val pageStr = JsonMethods.pretty(JsonMethods.render(page))

    // Compute the relative path path to the JSON file.
    val path = if (ns.isEmpty) "./index.json" else "./" + ns.mkString(".") + ".json"
    s"""<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |    <meta charset="UTF-8">
       |    <title>$title</title>
       |    <link href="__app__.css" rel="stylesheet" type="text/css"/>
       |    <link href="https://fonts.googleapis.com/css?family=Source+Code+Pro" rel="stylesheet">
       |    <link href="https://fonts.googleapis.com/css?family=Droid+Sans+Mono" rel="stylesheet">
       |    <link href="https://fonts.googleapis.com/css?family=Oswald" rel="stylesheet">
       |    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
       |</head>
       |<body>
       |
        |<!-- Application Element -->
       |<div id="app">
       |  <div id="navbar"></div>
       |</div>
       |
        |<!-- Menu Data -->
       |<script type="application/ecmascript">
       |window.menu = $menuStr;
       |</script>
       |
        |<!-- Page Data -->
       |<script type="application/ecmascript">
       |window.page = $pageStr;
       |</script>
       |
        |<!-- JavaScript Resource -->
       |<script src="__app__.js" type="application/ecmascript">
       |</script>
       |
        |<!-- Trigger Boot -->
       |<script type="application/ecmascript">
       |    bootstrap("$path");
       |</script>
       |
        |</body>
       |</html>
   """.stripMargin
  }

}
