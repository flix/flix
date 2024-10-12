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

import ca.uwaterloo.flix.language.ast.Range
import ca.uwaterloo.flix.api.lsp.{CodeLens, Command, Index, ResponseStatus}
import ca.uwaterloo.flix.language.ast.TypedAst.{Root, Spec}
import ca.uwaterloo.flix.language.ast.{Ast, SourceLocation, Symbol, Type, TypeConstructor}
import org.json4s.JsonAST.{JArray, JObject, JString}
import org.json4s.JsonDSL.*

object CodeLensProvider {

  /**
    * Processes a codelens request.
    */
  def processCodeLens(uri: String)(implicit root: Root): JObject = {
    val codeLenses = getRunCodeLenses(uri) ::: getTestCodeLenses(uri)
    ("status" -> ResponseStatus.Success) ~ ("result" -> JArray(codeLenses.map(_.toJSON)))
  }

  /**
    * Returns code lenses for running entry points.
    */
  private def getRunCodeLenses(uri: String)(implicit root: Root): List[CodeLens] = {
    getEntryPoints(uri)(root).map {
      case sym =>
        val args = List(JString(sym.toString))
        val command = Command("▶ Run", "flix.runMain", args)
        val range = sym.loc.range
        CodeLens(range, Some(command))
    }
  }

  /**
    * Returns code lenses for running tests.
    */
  private def getTestCodeLenses(uri: String)(implicit root: Root): List[CodeLens] = {
    getTests(uri)(root).map {
      case sym =>
        val command = Command("▶ Run Tests", "flix.cmdTests", Nil)
        val range = sym.loc.range
        CodeLens(range, Some(command))
    }
  }

  /**
    * Returns all entry points in the given `uri`.
    */
  private def getEntryPoints(uri: String)(implicit root: Root): List[Symbol.DefnSym] = root.defs.foldLeft(List.empty[Symbol.DefnSym]) {
    case (acc, (sym, defn)) if matchesUri(uri, sym.loc) && isEntryPoint(defn.spec) => defn.sym :: acc
    case (acc, _) => acc
  }

  /**
    * Returns all tests in the given `uri`.
    */
  private def getTests(uri: String)(implicit root: Root): List[Symbol.DefnSym] = root.defs.foldLeft(List.empty[Symbol.DefnSym]) {
    case (acc, (sym, defn)) if matchesUri(uri, sym.loc) && isEntryPoint(defn.spec) && isTest(defn.spec) => defn.sym :: acc
    case (acc, _) => acc
  }

  /**
    * Returns `true` if the given `spec` is an entry point.
    */
  private def isEntryPoint(s: Spec): Boolean = s.fparams match {
    case fparam :: Nil => isUnitType(fparam.tpe) && isPublic(s)
    case _ => false
  }

  /**
    * Returns `true` if the given `defn` is marked as a test.
    */
  private def isTest(s: Spec): Boolean = s.ann.isTest

  /**
    * Returns `true` if the given type `tpe` is the Unit type.
    */
  private def isUnitType(tpe: Type): Boolean = tpe.typeConstructor match {
    case Some(TypeConstructor.Unit) => true
    case _ => false
  }

  /**
    * Returns `true` if the given `defn` is marked as public
    */
  private def isPublic(spec: Spec): Boolean = {
    spec.mod.isPublic
  }

  /**
    * Returns `true` if the given source location `loc` matches the given `uri`.
    */
  private def matchesUri(uri: String, loc: SourceLocation): Boolean = uri == loc.source.name

}
