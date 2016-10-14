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

import ca.uwaterloo.flix.language.ast.Symbol.TableSym
import ca.uwaterloo.flix.language.ast.TypedAst.Table.Relation
import ca.uwaterloo.flix.language.ast.{Symbol, Type, TypedAst}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

object Documentor {

  /**
    * Generates documentation for the given program `p`.
    */
  def document(p: TypedAst.Root): Unit = {

    val defnsByNS = p.definitions.groupBy(_._1.namespace)
    val enumsByNS = p.enums.groupBy(_._1.namespace)
    val tablesByNS = p.tables.groupBy(_._1.namespace)
    val relationsByNS = tablesByNS.map {
      case (ns, m) => ns -> m.collect {
        case (sym, t: TypedAst.Table.Relation) => t
      }.toList
    }
    val latticesByNS = tablesByNS.map {
      case (ns, m) => ns -> m.collect {
        case (sym, t: TypedAst.Table.Lattice) => t
      }.toList
    }

    val namespaces = defnsByNS.keySet

    // Compute the data object for each namespace.
    val data = namespaces map {
      case ns =>

        val enums = enumsByNS.getOrElse(ns, Nil).toList.map(kv => mkEnum(kv._2))
        val relations = relationsByNS.getOrElse(ns, Nil) map mkRelation
        val lattices = latticesByNS.getOrElse(ns, Nil) map mkLattice

        ns -> JObject(
          JField("namespace", JString(ns.mkString("."))),
          JField("types", JArray(enums)),
          JField("definitions", mkDefinitions(defnsByNS(ns))),
          JField("relations", JArray(relations)),
          JField("lattices", JArray(lattices))
        )

    }

    // Process the menu.
    writeJSON(mkMenu(namespaces), getMenuPath)

    // Process each namespace.
    for ((ns, json) <- data) {
      writeString(mkHtmlPage(ns), getHtmlPath(ns))
      writeJSON(json, getJsonPath(ns))
    }

  }

  /**
    * Returns a JSON object of the available namespaces for the menu.
    */
  private def mkMenu(xs: Set[List[String]]): JArray = JArray(xs.toList.map {
    case ns => JObject(JField("name", JString(ns.mkString("."))))
  })

  /**
    * Returns a JSON object of all the given definitions.
    */
  private def mkDefinitions(m: Map[Symbol.DefnSym, Declaration.Definition]): JArray = JArray(
    m.map {
      case (sym, defn) =>

        val fparams = defn.formals.map {
          case TypedAst.FormalParam(psym, tpe, loc) =>
            JObject(
              JField("name", JString(psym.text)),
              JField("tpe", JString(prettyType(tpe)))
            )
        }

        // TODO: tparams
        //val tparams = defn.

        JObject(List(
          JField("name", JString(sym.name)),
          JField("tparams", JArray(List(JObject(JField("name", JString("a")))))),
          JField("fparams", JArray(fparams)),
          JField("result", JString(prettyType(getReturnType(defn.tpe)))),
          JField("comment", JString("A nice comment"))
        ))
    }.toList
  )

  /**
    * Returns the given enum `e` as a JSON object.
    */
  def mkEnum(e: Declaration.Enum): JObject = {
    JObject(List(
      JField("name", JString(e.sym.name)),
      JField("comment", JString("Some comment"))
    ))
  }

  /**
    * Returns the given relation `r` as a JSON object.
    */
  def mkRelation(r: Table.Relation): JObject = {
    val attributes = r.attributes.map {
      case TypedAst.Attribute(name, tpe, loc) => JObject(List(
        JField("name", JString(name)),
        JField("tpe", JString(prettyType(tpe)))
      ))
    }

    JObject(List(
      JField("name", JString(r.sym.name)),
      JField("attributes", JArray(attributes)),
      JField("comment", JString("Some comment"))
    ))
  }

  /**
    * Returns the given lattice `l` as a JSON object.
    */
  def mkLattice(l: Table.Lattice): JObject = {
    val attributes = (l.keys ::: l.value :: Nil).map {
      case TypedAst.Attribute(name, tpe, loc) => JObject(List(
        JField("name", JString(name)),
        JField("tpe", JString(prettyType(tpe)))
      ))
    }

    JObject(List(
      JField("name", JString(l.sym.name)),
      JField("attributes", JArray(attributes)),
      JField("comment", JString("Some comment"))
    ))
  }

  /**
    * Returns the path where the JSON menu file should be stored.
    */
  private def getMenuPath: Path = Paths.get("./build/api/__menu__.json")

  /**
    * Returns the path where the JSON file, for the given namespace, should be stored.
    */
  private def getJsonPath(ns: List[String]): Path = {
    if (ns.isEmpty)
      Paths.get("./build/api/index.json")
    else
      Paths.get("./build/api/" + ns.mkString(".") + ".json")
  }

  /**
    * Returns the path where the HTML file, for the given namespace, should be stored.
    */
  private def getHtmlPath(ns: List[String]): Path = {
    if (ns.isEmpty)
      Paths.get("./build/api/index.html")
    else
      Paths.get("./build/api/" + ns.mkString(".") + ".html")
  }

  /**
    * Writes the given JSON value `v` to the given path `p`.
    */
  private def writeJSON(v: JValue, p: Path): Unit = writeString(JsonMethods.pretty(JsonMethods.render(v)), p)

  /**
    * Writes the given string `s` to the given path `p`.
    */
  private def writeString(s: String, p: Path): Unit = try {
    val writer = Files.newBufferedWriter(p)
    writer.write(s)
    writer.close()
  } catch {
    case ex: IOException => throw new RuntimeException(s"Unable to write JSON to path '$p'.", ex)
  }

  /**
    * Converts the given type into a pretty string.
    */
  private def prettyType(t: Type): String = t.toString

  /**
    * Extracts the return type of the given arrow type `tpe`.
    */
  private def getReturnType(tpe: Type): Type = tpe match {
    case Type.Apply(Type.Arrow(l), ts) => ts.last
    case _ => throw InternalCompilerException(s"Unexpected type: '$tpe'.")
  }

  /**
    * Returns the HTML fragment to use for the given namespace `ns`.
    */
  private def mkHtmlPage(ns: List[String]): String = {
    s"""
       |<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |    <meta charset="UTF-8">
       |    <title>Flix Standard Library</title>
       |    <link href="css/stylesheet.css" rel="stylesheet" type="text/css"/>
       |    <link href="https://fonts.googleapis.com/css?family=Source+Code+Pro" rel="stylesheet">
       |    <link href="https://fonts.googleapis.com/icon?family=Material+Icons" rel="stylesheet">
       |</head>
       |<body>
       |<div id="app"></div>
       |<script src="js/bundle.js"></script>
       |<script type="application/ecmascript">
       |    bootstrap("./${ns.mkString(".")}.json");
       |</script>
       |</body>
       |</html>
   """.stripMargin
  }

}
