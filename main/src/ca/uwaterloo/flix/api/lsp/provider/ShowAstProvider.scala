/*
 * Copyright 2022 Jonathan Lindegaard Starup
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

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.Flix.IrFileExtension
import ca.uwaterloo.flix.api.lsp.Index
import ca.uwaterloo.flix.language.ast.TypedAst.Root
import ca.uwaterloo.flix.language.dbg.AstPrinter
import org.json4s.JsonAST.JObject
import org.json4s.JsonDSL._

object ShowAstProvider {

  /**
    * Returns a JSON object with
    *
    * - `title` (a string like `Namer.flix.ir`)
    * - `text` (a string with the ir representation).
    */
  def showAst(phase: String)(implicit index: Index, root: Option[Root], flix: Flix): JObject = root match {
    case None =>
      val text = "No IR available. Does the program not compile?"
      astObject(phase, text)
    case Some(r) =>
      // We have to compile the program to obtain the relevant AST.
      flix.codeGen(r)

      val phases = List("Parser", "Weeder", "Kinder", "Resolver", "TypedAst",
        "Documentor", "Lowering", "EarlyTreeShaker", "Monomorph",
        "MonomorphEnums", "Simplifier", "ClosureConv", "LambdaLift", "Tailrec",
        "Optimizer", "LateTreeShaker", "Reducer", "VarNumbering", "MonoTyper",
        "Eraser")

      phase match {
        case "Parser" => astObject(phase, "Work In Progress")
        case "Weeder" => astObject(phase, "Work In Progress")
        case "Kinder" => astObject(phase, "Work In Progress")
        case "Resolver" => astObject(phase, "Work In Progress")
        case "TypedAst" => astObject(phase, "Work In Progress")
        case "Documentor" => astObject(phase, "Work In Progress")
        case "Lowering" => astObject(phase, AstPrinter.formatLoweredAst(flix.getLoweringAst))
        case "EarlyTreeShaker" => astObject(phase, AstPrinter.formatLoweredAst(flix.getEarlyTreeShakerAst))
        case "Monomorph" => astObject(phase, AstPrinter.formatLoweredAst(flix.getMonomorphAst))
        case "MonomorphEnums" => astObject(phase, AstPrinter.formatLoweredAst(flix.getMonomorphEnumsAst))
        case "Simplifier" => astObject(phase, AstPrinter.formatSimplifiedAst(flix.getSimplifierAst))
        case "ClosureConv" => astObject(phase, AstPrinter.formatSimplifiedAst(flix.getClosureConvAst))
        case "LambdaLift" => astObject(phase, AstPrinter.formatLiftedAst(flix.getLambdaLiftAst))
        case "Tailrec" => astObject(phase, AstPrinter.formatLiftedAst(flix.getTailrecAst))
        case "Optimizer" => astObject(phase, AstPrinter.formatLiftedAst(flix.getOptimizerAst))
        case "LateTreeShaker" => astObject(phase, AstPrinter.formatLiftedAst(flix.getLateTreeShakerAst))
        case "Reducer" => astObject(phase, AstPrinter.formatReducedAst(flix.getReducerAst))
        case "VarNumbering" => astObject(phase, AstPrinter.formatReducedAst(flix.getVarNumberingAst))
        case _ =>
          astObject(phase, s"Unknown phase: '$phase'. Try one of these ${phases.map(s => s"'$s'").mkString(", ")}")
      }
  }

  private def astObject(phase: String, text: String): JObject = {
    ("title" -> s"$phase.$IrFileExtension") ~ ("text" -> text)
  }
}
