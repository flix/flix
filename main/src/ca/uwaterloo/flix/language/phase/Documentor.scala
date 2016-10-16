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

import ca.uwaterloo.flix.language.ast.{Ast, Type}
import ca.uwaterloo.flix.language.ast.TypedAst._
import ca.uwaterloo.flix.util.InternalCompilerException
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

object Documentor {

  /**
    * Generates documentation for the given program `p`.
    */
  def document(p: Root): Unit = {
    // Group definitions, enums, and tables by their namespace.
    val defnsByNS = p.definitions.groupBy(_._1.namespace)
    val enumsByNS = p.enums.groupBy(_._1.namespace)
    val tablesByNS = p.tables.groupBy(_._1.namespace)

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
    val namespaces = defnsByNS.keySet ++ enumsByNS.keySet ++ tablesByNS.keySet

    // Process each namespace.
    val data = namespaces map {
      case ns =>
        val defns = defnsByNS.getOrElse(ns, Nil).toList.map(kv => mkDefn(kv._2))
        val enums = enumsByNS.getOrElse(ns, Nil).toList.map(kv => mkEnum(kv._2))
        val relations = relationsByNS.getOrElse(ns, Nil) map mkRelation
        val lattices = latticesByNS.getOrElse(ns, Nil) map mkLattice

        ns -> JObject(
          JField("namespace", JString(ns.mkString("."))),
          JField("types", JArray(enums)),
          JField("definitions", JArray(defns)),
          JField("relations", JArray(relations)),
          JField("lattices", JArray(lattices))
        )
    }

    // Process the menu.
    writeJSON(JArray(mkMenu(namespaces.filter(_.nonEmpty))), getMenuPath)

    // Write the result for each namespace.
    for ((ns, json) <- data) {
      writeString(mkHtmlPage(ns), getHtmlPath(ns))
      writeJSON(json, getJsonPath(ns))
    }
  }

  /**
    * Returns a JSON object of the available namespaces for the menu.
    */
  private def mkMenu(xs: Set[List[String]]): List[JObject] = xs.toList.map {
    case ns => JObject(JField("name", JString(ns.mkString("."))))
  }

  /**
    * Returns the given definition `d` as a JSON object.
    */
  private def mkDefn(d: Declaration.Definition): JObject = {
    // Process type parameters.
    val tparams = d.tparams.map {
      case TypeParam(ident, tpe, loc) => JObject(List(
        JField("name", JString(ident.name))
      ))
    }

    // Process formal parameters.
    val fparams = d.formals.map {
      case FormalParam(psym, tpe, loc) => JObject(
        JField("name", JString(psym.text)),
        JField("tpe", JString(prettify(tpe)))
      )
    }

    // Compute return type.
    val returnType = prettify(getReturnType(d.tpe))

    JObject(List(
      JField("name", JString(d.sym.name)),
      JField("tparams", JArray(tparams)),
      JField("fparams", JArray(fparams)),
      JField("result", JString(returnType)),
      JField("comment", JString(getComment(d.doc)))
    ))

  }

  /**
    * Returns the given enum `e` as a JSON object.
    */
  private def mkEnum(e: Declaration.Enum): JObject = {
    JObject(List(
      JField("name", JString(e.sym.name)),
      JField("comment", JString("Some comment"))
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
      JField("comment", JString("Some comment"))
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
  private def prettify(t: Type): String = t.toString

  /**
    * Extract the comment from the given optional documentation.
    */
  private def getComment(o: Option[Ast.Documentation]): String = o match {
    case None => ""
    case Some(doc) => doc.text
  }

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
    // Compute the relative path path to the JSON file.
    val path = if (ns.isEmpty) "./index.json" else "./" + ns.mkString(".") + ".json"
    s"""<!DOCTYPE html>
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
        |    bootstrap("$path");
        |</script>
        |</body>
        |</html>
   """.stripMargin
  }

}
