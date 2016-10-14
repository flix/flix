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
import java.nio.charset.Charset
import java.nio.file.{Files, Path, Paths}

import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.language.ast.TypedAst
import ca.uwaterloo.flix.language.ast.TypedAst._
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

object Documentor {

  /**
    * The path to output the JSON menu to.
    */
  val MenuPath = Paths.get("./build/api/js/__menu__.json")

  /**
    * Generates documentation for the given program `p`.
    */
  def document(p: TypedAst.Root): Unit = {

    // Retrieve each definition by its namespace.
    val definitionsByNamespace = p.definitions.groupBy(_._1.namespace)

    // Compute the set of all namespaces.
    val namespaces = definitionsByNamespace.keySet

    // Compute the data object for each namespace.
    val data = namespaces map {
      case ns => ns -> JObject(
        JField("namespace", JString(ns.mkString("."))),
        JField("types", JArray(List())),
        JField("definitions", mkDefinitions(definitionsByNamespace(ns))),
        JField("relations", JArray(List())),
        JField("lattices", JArray(List()))
      )

    }

    // Process the menu.
    writeJSON(mkMenu(namespaces), MenuPath)

    // Process each namespace.
    for ((ns, json) <- data) {
      val str = JsonMethods.pretty(JsonMethods.render(data.head._2))
      val path = Paths.get("./build/api/js/" + ns.mkString(".") + ".json")
      val writer = Files.newBufferedWriter(path)
      writer.write(str)
      writer.close()
    }

  }

  /**
    * Returns a JSON object of the available namespaces for the menu.
    */
  private def mkMenu(xs: Set[List[String]]): JArray = JArray(xs.toList.map {
    case ns => JObject(JField("name", JString(ns.mkString("."))))
  })

  private def mkDefinitions(m: Map[Symbol.DefnSym, Declaration.Definition]): JArray = JArray(
    m.map {
      case (sym, defn) =>
        JObject(List(
          JField("name", JString(sym.name)),
          JField("tparams", JArray(List(JObject(JField("name", JString("a")))))),
          JField("fparams", JArray(List(JObject(JField("name", JString("a")), JField("tpe", JString("someType")))))),
          JField("result", JString("result")),
          JField("comment", JString("A nice comment"))
        ))
    }.toList
  )

  /**
    * Writes the given JSON value `v` to the given path `p`.
    */
  private def writeJSON(v: JValue, p: Path): Unit = try {
    val text = JsonMethods.pretty(JsonMethods.render(v))
    val writer = Files.newBufferedWriter(p)
    writer.write(text)
    writer.close()
  } catch {
    case ex: IOException => throw new RuntimeException(s"Unable to write JSON to path '$p'.", ex)
  }

  private def mkHtmlPage(): String =
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
       |</body>
       |</html>
       |
   """.stripMargin

}
