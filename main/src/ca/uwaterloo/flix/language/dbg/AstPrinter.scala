/*
 * Copyright 2023 Jonathan Lindegaard Starup
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

package ca.uwaterloo.flix.language.dbg

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.api.Flix.{IrFileExtension, IrFileIndentation, IrFileWidth}
import ca.uwaterloo.flix.language.ast._
import ca.uwaterloo.flix.language.dbg.printer._
import ca.uwaterloo.flix.util.{FileOps, InternalCompilerException}

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /**
    * Writes all the formatted asts, requested by the flix options, to disk.
    */
  def printAsts()(implicit flix: Flix): Unit = {
    val optionPhases = flix.options.xprintphase
    val shouldPrintEverything = optionPhases.contains("all") || optionPhases.contains("All")
    val phaseMap = if (shouldPrintEverything) allPhases(includeUnfinished = false)
                   else allPhases().filter(pair => optionPhases.contains(pair._1))
    printPhaseMap(phaseMap)
  }

  /**
    * Writes all the formatted asts to disk.
    */
  def printAllAsts()(implicit flix: Flix): Unit = {
    printPhaseMap(allPhases(includeUnfinished = false))
  }

  /**
    * Goes through each map binding and calls [[writeToDisk]].
    */
  private def printPhaseMap(phaseMap: Map[String, () => String])(implicit flix: Flix): Unit = {
    for ((phase, printer) <- phaseMap)
      writeToDisk(phase, printer())
  }

  /**
    * Returns a list of map the phases of flix along with a thunked pretty printed AST.
    *
    * Returns only the phases that can be pretty printed if `includeUnfinished` is false.
    */
  def allPhases(includeUnfinished: Boolean = true)(implicit flix: Flix): Map[String, () => String] = {
    def wipPhase(phaseName: String): Option[(String, () => String)] = {
      if (includeUnfinished) Some((phaseName, () => "Work In Progress")) else None
    }

    val frontend = List(
      wipPhase("Parser"),
      wipPhase("Weeder"),
      wipPhase("Desugar"),
      wipPhase("Namer"),
      wipPhase("Resolver"),
      wipPhase("Kinder"),
      wipPhase("Deriver"),
      Some("Typer", () => formatTypedAst(flix.getTyperAst)),
      wipPhase("Entrypoint"),
      wipPhase("PredDeps"),
      wipPhase("Stratifier"),
      wipPhase("PatMatch"),
      wipPhase("Redundancy"),
      wipPhase("Safety")
    ).flatten.toMap
    val backend = List(
      Some(("Lowering", () => formatLoweredAst(flix.getLoweringAst))),
      Some(("TreeShaker1", () => formatLoweredAst(flix.getTreeShaker1Ast))),
      wipPhase("MonoDefs"),
      wipPhase("MonoTypes"),
      Some(("Simplifier", () => formatSimplifiedAst(flix.getSimplifierAst))),
      Some(("ClosureConv", () => formatSimplifiedAst(flix.getClosureConvAst))),
      Some(("LambdaLift", () => formatLiftedAst(flix.getLambdaLiftAst))),
      Some(("Optimizer", () => formatLiftedAst(flix.getOptimizerAst))),
      Some(("TreeShaker2", () => formatLiftedAst(flix.getTreeShaker2Ast))),
      Some(("EffectBinder", () => formatReducedAst(flix.getEffectBinderAst))),
      Some(("Tailrec", () => formatReducedAst(flix.getTailrecAst))),
      Some(("Eraser", () => formatReducedAst(flix.getEraserAst))),
      Some(("Reducer", () => formatReducedAst(flix.getReducerAst))),
      Some(("VarOffsets", () => formatReducedAst(flix.getVarOffsetsAst))),
      wipPhase("JvmBackend")
    ).flatten.toMap
    frontend ++ backend
  }

  /**
    * Formats `root` for display.
    */
  def formatTypedAst(root: TypedAst.Root): String = {
    formatDocProgram(TypedAstPrinter.print(root))
  }


  /**
    * Formats `root` for display.
    */
  def formatLoweredAst(root: LoweredAst.Root): String = {
    formatDocProgram(LoweredAstPrinter.print(root))
  }

  /**
    * Formats `root` for display.
    */
  def formatSimplifiedAst(root: SimplifiedAst.Root): String = {
    formatDocProgram(SimplifiedAstPrinter.print(root))
  }

  /**
    * Formats `root` for display.
    */
  def formatLiftedAst(root: LiftedAst.Root): String = {
    formatDocProgram(LiftedAstPrinter.print(root))
  }

  /**
    * Formats `root` for display.
    */
  def formatReducedAst(root: ReducedAst.Root): String = {
    formatDocProgram(ReducedAstPrinter.print(root))
  }

  /**
    * Formats `p` as a [[String]].
    */
  private def formatDocProgram(p: DocAst.Program): String = {
    implicit val i: Doc.Indent = Doc.indentationLevel(IrFileIndentation)
    val docs = DocAstFormatter.format(p)
    docs.map(Doc.pretty(IrFileWidth, _)).mkString("\n\n")
  }

  /**
    * Writes `content` to the file `./build/asts/<fileName>.flixir`. The build folder is taken from
    * flix options if present. The existing file is overwritten if present.
    */
  private def writeToDisk(fileName: String, content: String)(implicit flix: Flix): Unit = {
    val buildAstsPath = flix.options.output.getOrElse(Path.of("./build/")).resolve("asts/")
    val filePath = buildAstsPath.resolve(s"$fileName.$IrFileExtension")
    Files.createDirectories(buildAstsPath)

    // Check if the file already exists.
    if (Files.exists(filePath)) {
      // Check that the file is a regular file.
      if (!Files.isRegularFile(filePath, LinkOption.NOFOLLOW_LINKS)) {
        throw InternalCompilerException(s"Unable to write to non-regular file: '$filePath'.", SourceLocation.Unknown)
      }

      // Check if the file is writable.
      if (!Files.isWritable(filePath)) {
        throw InternalCompilerException(s"Unable to write to read-only file: '$filePath'.", SourceLocation.Unknown)
      }
    }
    FileOps.writeString(filePath, content)
  }

}
