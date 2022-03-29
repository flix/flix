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
import ca.uwaterloo.flix.language.ast.TypedAst.{Def, Root, Spec}
import ca.uwaterloo.flix.language.ast.{SourceLocation, Type, TypeConstructor}
import org.json4s.JsonAST.{JArray, JObject, JString}
import org.json4s.JsonDSL._

object CodeLensProvider {

  // TODO: Doc
  case class EntryPoint(defn: Def, withArgs: Boolean)

  /**
    * Processes a codelens request.
    */
  def processCodeLens(uri: String)(implicit index: Index, root: Root): JObject = {
    val codeLenses = getRunCodeLenses(uri)
    ("status" -> "success") ~ ("result" -> JArray(codeLenses.map(_.toJSON)))
  }

  /**
    * Returns code lenses for all possible entry points.
    */
  private def getRunCodeLenses(uri: String)(implicit index: Index, root: Root): List[CodeLens] = {
    if (root == null) {
      return Nil
    }

    getEntryPoints(uri).flatMap {
      case EntryPoint(defn, withArgs) =>
        val args = List(JString(defn.sym.toString))
        val runMain = Command("Run", "flix.runMain", args)
        val runMainWithArgs = Command("Run with args...", "flix.runMainWithArgs", args)
        val runMainNewTerminal = Command("Run (in new terminal)", "flix.runMainNewTerminal", args)
        val runMainNewTerminalWithArgs = Command("Run with args... (in new terminal)", "flix.runMainNewTerminalWithArgs", args)
        val range = Range.from(defn.sym.loc)

        if (!withArgs)
          List(
            CodeLens(range, Some(runMain)),
            CodeLens(range, Some(runMainNewTerminal)),
          )
        else
          List(
            CodeLens(range, Some(runMain)),
            CodeLens(range, Some(runMainWithArgs)),
            CodeLens(range, Some(runMainNewTerminal)),
            CodeLens(range, Some(runMainNewTerminalWithArgs))
          )
    }
  }

  /**
    * Returns all entry points in the given `uri`.
    */
  private def getEntryPoints(uri: String)(implicit root: Root): List[EntryPoint] = root.defs.foldLeft(List.empty[EntryPoint]) {
    case (acc, (sym, defn)) if matchesUri(uri, sym.loc) && isEntryPointWithNoArgs(defn.spec) => EntryPoint(defn, withArgs = false) :: acc
    case (acc, (sym, defn)) if matchesUri(uri, sym.loc) && isEntryPointWithArgs(defn.spec) => EntryPoint(defn, withArgs = true) :: acc
    case (acc, _) => acc
  }

  /**
    * Returns `true` if the given `spec` is an entry point and takes no arguments.
    */
  private def isEntryPointWithNoArgs(s: Spec): Boolean = s.fparams match {
    case fparam :: Nil => isUnitType(fparam.tpe) || isStringArray(fparam.tpe)
    case _ => false
  }

  /**
    * Returns `true` if the given `spec` is an entry point and takes arguments.
    */
  private def isEntryPointWithArgs(s: Spec): Boolean = s.fparams match {
    case fparam :: Nil => isUnitType(fparam.tpe) || isStringArray(fparam.tpe)
    case _ => false
  }

  /**
    * Returns `true` if the given type `tpe` is the Unit type.
    */
  private def isUnitType(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Unit) => true
    case _ => false
  }

  /**
    * Returns `true` if the given type `tpe` is the Array[String] type.
    */
  private def isStringArray(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.ScopedArray) => tpe.typeArguments.headOption.contains(Type.Str)
    case _ => false
  }

  /**
    * Returns `true` if the given source location `loc` matches the given `uri`.
    */
  private def matchesUri(uri: String, loc: SourceLocation): Boolean = uri == loc.source.name

}
