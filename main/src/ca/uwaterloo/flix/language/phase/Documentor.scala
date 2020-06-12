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
import ca.uwaterloo.flix.language.ast.{Type, TypeConstructor, TypedAst}
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._
import org.json4s.JsonAST._
import org.json4s.native.JsonMethods

import scala.annotation.tailrec

object Documentor extends Phase[TypedAst.Root, TypedAst.Root] {

  /**
    * The title of the generated API.
    */
  val ApiTitle = "Flix Standard Library"

  /**
    * The directory where to write the ouput.
    */
  val OutputDirectory: Path = Paths.get("./target/api")

  /**
    * Emits a JSON file with information about the definitions of the program.
    */
  def run(root: TypedAst.Root)(implicit flix: Flix): Validation[TypedAst.Root, CompilationError] = flix.phase("Documentor") {
    // Check whether to generate documentation.
    if (flix.options.documentor) {
      // Collect all public definitions and group them by namespace.
      val defsByNS = root.defs.filter {
        case (sym, defn) => defn.mod.isPublic && !defn.ann.isBenchmark && !defn.ann.isLaw && !defn.ann.isTest
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
      val p = OutputDirectory.resolve("api.js")

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
    val tparams = defn0.tparams.map {
      case TypeParam(ident, tpe, loc) => JObject(List(
        JField("name", JString(ident.name))
      ))
    }

    // Compute the formal parameters.
    val fparams = getFormalParams(defn0).collect {
      case FormalParam(psym, mod, tpe, loc) if tpe != Type.Cst(TypeConstructor.Unit) => JObject(
        JField("name", JString(psym.text)),
        JField("type", JString(format(tpe)))
      )
    }

    // Compute return type.
    val returnType = getReturnType(defn0.inferredScheme.base)

    // Construct the JSON object.
    JObject(List(
      JField("name", JString(defn0.sym.name)),
      JField("tparams", JArray(tparams)),
      JField("fparams", JArray(fparams)),
      JField("result", JString(format(returnType))),
      JField("comment", JString(defn0.doc.text.trim))
    ))
  }

  /**
    * Returns the (uncurried) formal parameters of the given definition `defn0`.
    */
  private def getFormalParams(defn0: Def): List[FormalParam] = {
    /**
      * Returns the formal parameters of the lambda expressions in the given expression `exp0`.
      */
    def uncurry(exp0: Expression): List[FormalParam] = exp0 match {
      case Expression.Lambda(fparam, exp, _, _) => fparam :: uncurry(exp)
      case _ => Nil
    }

    defn0.fparams ::: uncurry(defn0.exp)
  }

  /**
    * Returns the return type of the given function type `tpe0`.
    */
  @tailrec
  private def getReturnType(tpe0: Type): Type = tpe0 match {
    case Type.Apply(Type.Apply(Type.Arrow(_, _), _), tpe) => getReturnType(tpe)
    case _ => tpe0
  }

  /**
    * Returns a string representation of the given type `tpe0`.
    */
  private def format(tpe0: Type): String = {
    val base = tpe0.typeConstructor
    val args = tpe0.typeArguments

    base match {
      case tvar: Type.Var => tvar.getText.getOrElse(tvar.id.toString)

      case Type.Cst(tc) => tc match {
        case TypeConstructor.Unit => "Unit"
        case TypeConstructor.Bool => "Bool"
        case TypeConstructor.Char => "Char"
        case TypeConstructor.Float32 => "Float32"
        case TypeConstructor.Float64 => "Float64"
        case TypeConstructor.Int8 => "Int8"
        case TypeConstructor.Int16 => "Int16"
        case TypeConstructor.Int32 => "Int32"
        case TypeConstructor.Int64 => "Int64"
        case TypeConstructor.BigInt => "BigInt"
        case TypeConstructor.Str => "Str"
        case TypeConstructor.RecordEmpty => "{ }"
        case TypeConstructor.SchemaEmpty => "Schema { }"

        case TypeConstructor.Relation => "Relation"

        case TypeConstructor.Lattice => "Lattice"

        case TypeConstructor.Array => "Array" + "[" + args.map(format).mkString(", ") + "]"

        case TypeConstructor.Channel => "Channel" + "[" + args.map(format).mkString(", ") + "]"

        case TypeConstructor.Enum(sym, kind) =>
          if (args.isEmpty)
            sym.toString
          else
            sym.toString + "[" + args.map(format).mkString(", ") + "]"

        case TypeConstructor.Tag(sym, tag) => sym.toString + "." + tag

        case TypeConstructor.RecordExtend(label) =>
          "{" + label + " = " + format(args(0)) + " | " + format(args(1)) + "}"

        case TypeConstructor.SchemaExtend(sym) =>
          "{" + sym + " = " + format(args(0)) + " | " + format(args(1)) + "}"

        case TypeConstructor.Native(clazz) => clazz.getName + (if (args.isEmpty) "" else "[" + args.map(format).mkString(", ") + "]")

        case TypeConstructor.Ref => "Ref" + "[" + args.map(format).mkString(", ") + "]"

        case TypeConstructor.Tuple(l) => "(" + args.map(format).mkString(", ") + ")"

        case TypeConstructor.Pure => "Pure"

        case TypeConstructor.Impure => "Impure"

        case TypeConstructor.Not => "Not"

        case TypeConstructor.And => "And"

        case TypeConstructor.Or => "Or"
          
      }

      case Type.Arrow(l, _) =>
        val argumentTypes = args.init
        val resultType = args.last
        if (argumentTypes.length == 1) {
          format(argumentTypes.head) + " -> " + format(resultType)
        } else {
          "(" + argumentTypes.map(format).mkString(", ") + ") -> " + format(resultType)
        }

      case Type.Lambda(tvar, tpe) => tvar.toString + " => " + format(tpe)

      case Type.Apply(tpe1, tpe2) => format(tpe1) + "[" + format(tpe2) + "]"
    }
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
