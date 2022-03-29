/*
 * Copyright 2022 Magnus Madsen
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
package ca.uwaterloo.flix.api.lsp.provider

import ca.uwaterloo.flix.api.lsp.{CodeLens, Command, Index, Range}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Symbol, Type, TypeConstructor}
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root, Spec}
import org.json4s.JsonAST.{JArray, JObject}
import org.json4s.JsonDSL._

object CodeLensProvider {

  /**
    * Processes a codelens request.
    */
  def processCodeLens(uri: String)(implicit index: Index, root: Root): JObject = {
    //
    // Compute all code lenses.
    //
    val allCodeLenses = mkCodeLensForMain(uri) ::: mkCodeLensesForEntryPoints(uri)
    ("status" -> "success") ~ ("result" -> JArray(allCodeLenses.map(_.toJSON)))
  }

  /**
    * Returns code lenses for all possible entry points.
    */
  private def mkCodeLensesForEntryPoints(uri: String)(implicit index: Index, root: Root): List[CodeLens] = {
    if (root == null) {
      return Nil
    }

    getAllEntryPoints(uri).map {
      case defn =>
        val runMain = Command("Run TODO", "flix.runMain", Nil) // TODO
        CodeLens(Range.from(defn.spec.loc), Some(runMain))
    }
  }

  /**
    * Returns all entry points in the given `uri`.
    */
  private def getAllEntryPoints(uri: String)(implicit root: Root): List[Def] = root.defs.foldLeft(List.empty[Def]) {
    case (acc, (sym, defn)) if matchesUri(uri, sym.loc) && isEntryPoint(defn.spec) => defn :: acc
    case (acc, _) => acc
  }

  /**
    * Returns `true` if the given `spec` is an entry point.
    */
  private def isEntryPoint(s: Spec): Boolean = s.fparams match {
    case fparam :: Nil => isUnitType(fparam.tpe) || isStringArray(fparam.tpe)
    case _ => false
  }

  /**
    * Returns `true` if the given type `tpe` is the Unit type.
    */
  private def isUnitType(tpe: Type): Boolean = tpe match {
    case Type.Apply(Type.Cst(TypeConstructor.Unit, _), _, _) => true
    case _ => false
  }

  /**
    * Returns `true` if the given type `tpe` is the Array[String] type.
    */
  private def isStringArray(tpe: Type): Boolean = tpe match {
    case Type.Apply(Type.Apply(Type.Cst(TypeConstructor.ScopedArray, _), _, _), _, _) => true
    case _ => false
  }

  /**
    * Returns a code lens for main (if present).
    */
  private def mkCodeLensForMain(uri: String)(implicit index: Index, root: Root): List[CodeLens] = {
    if (root == null) {
      return Nil
    }

    val main = Symbol.Main
    root.defs.get(main) match {
      case Some(defn) if matchesUri(uri, defn.sym.loc) =>
        val runMain = Command("Run", "flix.runMain", Nil)
        val runMainWithArgs = Command("Run with args...", "flix.runMainWithArgs", Nil)
        val runMainNewTerminal = Command("Run (in new terminal)", "flix.runMainNewTerminal", Nil)
        val runMainNewTerminalWithArgs = Command("Run with args... (in new terminal)", "flix.runMainNewTerminalWithArgs", Nil)
        val loc = defn.sym.loc
        List(
          CodeLens(Range.from(loc), Some(runMain)),
          CodeLens(Range.from(loc), Some(runMainWithArgs)),
          CodeLens(Range.from(loc), Some(runMainNewTerminal)),
          CodeLens(Range.from(loc), Some(runMainNewTerminalWithArgs))
        )
      case _ => Nil
    }
  }

  /**
    * Returns `true` if the given source location `loc` matches the given `uri`.
    */
  private def matchesUri(uri: String, loc: SourceLocation): Boolean = uri == loc.source.name

}
