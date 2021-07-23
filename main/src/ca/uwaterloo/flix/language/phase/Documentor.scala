/*
 * Copyright 2019 Magnus Madsen
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
import ca.uwaterloo.flix.language.ast.ops.TypedAstOps._
import ca.uwaterloo.flix.language.ast.{Ast, Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.language.debug.{Audience, FormatExpression, FormatType, PrettyExpression}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import org.json4s.JsonAST._
import org.json4s.JsonDSL._
import org.json4s.native.JsonMethods

object Documentor extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * The title of the generated API.
    */
  val ApiTitle = "Flix Standard Library"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./target/api")

  private implicit val audience: Audience = Audience.External

  /**
    * Emits a JSON file with information about the definitions of the program.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Documentor") {
    // Check whether to generate documentation.
    if (flix.options.documentor) {
      // Collect all public definitions and group them by namespace.
      val defsByNS = root.defs.filter {
        case (sym, defn) => defn.spec.mod.isPublic && !isBenchmark(defn.spec.ann) && !isLaw(defn.spec.ann) && !isTest(defn.spec.ann)
      }.groupBy(_._1.namespace)

      // Convert all definitions to JSON objects.
      val jsonDefsByNs = defsByNS.foldRight(List.empty[(String, JObject)]) {
        case ((ns, defs), acc) =>
          val ds = defs.toList.map(kv => visitDef(kv._2))
          (ns.mkString(".") -> JObject(JField("defs", JArray(ds)))) :: acc
      }

      // Create the output directory (and its parent directories).
      Files.createDirectories(OutputDirectory)

      // Construct the JSON object.
      val json = JObject(
        ("title", JString(ApiTitle)),
        ("namespaces", JObject(jsonDefsByNs))
      )

      // Serialize the JSON object to a string.
      val s = JsonMethods.pretty(JsonMethods.render(json))

      // The path to the file to write.
      val p = OutputDirectory.resolve("api.json")

      // Write the string to the path.
      writeString(s, p)
    }

    root.toSuccess
  }

  /**
    * Returns the given definition `defn0` as a JSON object.
    */
  private def visitDef(defn0: Def): JObject = {
    // Compute the type parameters.
    val tparams = defn0.spec.tparams.map {
      case TypeParam(ident, tpe, loc) => JObject(List(
        JField("name", JString(ident.name))
      ))
    }

    // Compute the formal parameters.
    val fparams = defn0.spec.fparams.collect {
      case FormalParam(psym, mod, tpe, loc) if tpe != Type.Unit => JObject(
        JField("name", JString(psym.text)),
        JField("type", JString(FormatType.formatType(tpe)))
      )
    }

    // Compute return type and effect.
    val result = defn0.spec.tpe
    val effect = defn0.spec.eff

    // Construct the JSON object.
    ("name" -> defn0.sym.name) ~
      ("tparams" -> tparams) ~
      ("fparams" -> fparams) ~
      ("result" -> FormatType.formatType(result)) ~
      ("effect" -> FormatType.formatType(effect)) ~
      ("time" -> getTime(defn0)) ~
      ("space" -> getSpace(defn0)) ~
      ("comment" -> defn0.spec.doc.text.trim)
  }

  /**
    * Optionally returns the time complexity of the given definition `defn0`.
    */
  private def getTime(defn0: Def): Option[String] = defn0.spec.ann.collectFirst {
    case Annotation(Ast.Annotation.Time(_), exp :: _, _) =>
      PrettyExpression.pretty(exp)
  }

  /**
    * Optionally returns the space complexity of the given definition `defn0`.
    */
  private def getSpace(defn0: Def): Option[String] = defn0.spec.ann.collectFirst {
    case Annotation(Ast.Annotation.Space(_), exp :: _, _) =>
      PrettyExpression.pretty(exp)
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

}
