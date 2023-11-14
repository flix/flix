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
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /**
    * Writes all the formatted asts, requested by the flix options, to disk.
    */
  def printAsts()(implicit flix: Flix): Unit = {
    val asts = flix.options.xprintphase
    if (asts.isEmpty)
      ()
    else if (asts.contains("all") || asts.contains("All"))
      printAllAsts()
    else {
      //
      // Front-end
      //
      if (asts.contains("Parser")) () // wip
      if (asts.contains("Weeder")) () // wip
      if (asts.contains("Desugar")) () // wip
      if (asts.contains("Namer")) () // wip
      if (asts.contains("Resolver")) () // wip
      if (asts.contains("Kinder")) () // wip
      if (asts.contains("Deriver")) () // wip
      if (asts.contains("Typer")) () // wip
      if (asts.contains("Entrypoint")) () // wip
      if (asts.contains("PredDeps")) () // wip
      if (asts.contains("Stratifier")) () // wip
      if (asts.contains("PatMatch")) () // wip
      if (asts.contains("Redundancy")) () // wip
      if (asts.contains("Safety")) () // wip
      //
      // Back-end
      //
      if (asts.contains("Lowering")) writeToDisk("Lowering", formatLoweredAst(flix.getLoweringAst))
      if (asts.contains("TreeShaker1")) writeToDisk("TreeShaker1", formatLoweredAst(flix.getTreeShaker1Ast))
      if (asts.contains("MonoDefs")) writeToDisk("MonoDefs", formatLoweredAst(flix.getMonoDefsAst))
      if (asts.contains("MonoTypes")) writeToDisk("MonoTypes", formatLoweredAst(flix.getMonoTypesAst))
      if (asts.contains("Simplifier")) writeToDisk("Simplifier", formatSimplifiedAst(flix.getSimplifierAst))
      if (asts.contains("ClosureConv")) writeToDisk("ClosureConv", formatSimplifiedAst(flix.getClosureConvAst))
      if (asts.contains("LambdaLift")) writeToDisk("LambdaLift", formatLiftedAst(flix.getLambdaLiftAst))
      if (asts.contains("Tailrec")) writeToDisk("Tailrec", formatLiftedAst(flix.getTailrecAst))
      if (asts.contains("Optimizer")) writeToDisk("Optimizer", formatLiftedAst(flix.getOptimizerAst))
      if (asts.contains("TreeShaker2")) writeToDisk("TreeShaker2", formatLiftedAst(flix.getTreeShaker2Ast))
      if (asts.contains("Reducer")) writeToDisk("Reducer", formatReducedAst(flix.getReducerAst))
      if (asts.contains("VarOffsets")) writeToDisk("VarOffsets", formatReducedAst(flix.getVarOffsetsAst))
      if (asts.contains("JvmBackend")) () // wip
    }
  }

  /**
    * Writes all the formatted asts to disk.
    */
  def printAllAsts()(implicit flix: Flix): Unit = {
    //
    // Front-end
    //

    // Parser wip
    // Weeder wip
    // Desugar wip
    // Namer wip
    // Resolver wip
    // Kinder wip
    // Deriver wip
    // Typer wip
    // Entrypoint wip
    // PredDeps wip
    // Stratifier wip
    // PatMatch wip
    // Redundancy wip
    // Safety wip

    //
    // Back-end
    //

    writeToDisk("Lowering", formatLoweredAst(flix.getLoweringAst))
    writeToDisk("TreeShaker1", formatLoweredAst(flix.getTreeShaker1Ast))
    writeToDisk("MonoDefs", formatLoweredAst(flix.getMonoDefsAst))
    writeToDisk("MonoTypes", formatLoweredAst(flix.getMonoTypesAst))
    writeToDisk("Simplifier", formatSimplifiedAst(flix.getSimplifierAst))
    writeToDisk("ClosureConv", formatSimplifiedAst(flix.getClosureConvAst))
    writeToDisk("LambdaLift", formatLiftedAst(flix.getLambdaLiftAst))
    writeToDisk("Tailrec", formatLiftedAst(flix.getTailrecAst))
    writeToDisk("Optimizer", formatLiftedAst(flix.getOptimizerAst))
    writeToDisk("TreeShaker2", formatLiftedAst(flix.getTreeShaker2Ast))
    writeToDisk("Reducer", formatReducedAst(flix.getReducerAst))
    writeToDisk("VarOffsets", formatReducedAst(flix.getVarOffsetsAst))
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
    Files.write(filePath, content.getBytes)
  }

}
