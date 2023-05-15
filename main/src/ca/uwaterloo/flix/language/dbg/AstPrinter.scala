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
import ca.uwaterloo.flix.language.ast.{ErasedAst, MonoTypedAst, LiftedAst, SourceLocation}
import ca.uwaterloo.flix.language.dbg.printer.{ErasedAstPrinter, MonoTypedAstPrinter, LiftedAstPrinter}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /**
    * Writes all the formatted asts, requested by the flix options, to disk.
    */
  def printAsts()(implicit flix: Flix): Unit = {
    val asts = flix.options.xprintphaseasts
    if (asts.nonEmpty) {
      if (asts.contains("Parser")) () // wip
      if (asts.contains("Weeder")) () // wip
      if (asts.contains("Kinder")) () // wip
      if (asts.contains("Resolver")) () // wip
      if (asts.contains("TypedAst")) () // wip
      if (asts.contains("Documentor")) () // wip
      if (asts.contains("Lowering")) () // wip
      if (asts.contains("EarlyTreeShaker")) () // wip
      if (asts.contains("Monomorph")) () // wip
      if (asts.contains("Simplifier")) () // wip
      if (asts.contains("ClosureConv")) () // wip
      if (asts.contains("LambdaLift")) writeToDisk("LambdaLift", formatLiftedAst(flix.getLambdaLiftAst))
      if (asts.contains("Tailrec")) writeToDisk("Tailrec", formatLiftedAst(flix.getTailrecAst))
      if (asts.contains("Optimizer")) writeToDisk("Optimizer", formatLiftedAst(flix.getOptimizerAst))
      if (asts.contains("LateTreeShaker")) writeToDisk("LateTreeShaker", formatLiftedAst(flix.getLateTreeShakerAst))
      if (asts.contains("Reducer")) () // wip
      if (asts.contains("VarNumbering")) () // wip
      if (asts.contains("MonoTyper")) writeToDisk("MonoTyper", formatMonoTypedAst(flix.getMonoTyperAst))
      if (asts.contains("Eraser")) writeToDisk("Eraser", formatErasedAst(flix.getEraserAst))
    }
  }

  /**
    * Writes all the formatted asts to disk.
    */
  def printAllAsts()(implicit flix: Flix): Unit = {
    // Parser wip
    // Weeder wip
    // Kinder wip
    // Resolver wip
    // TypedAst wip
    // Documentor wip
    // Lowering wip
    // EarlyTreeShaker wip
    // Monomorph wip
    // Simplifier wip
    // ClosureConv wip
    writeToDisk("LambdaLift", formatLiftedAst(flix.getLambdaLiftAst))
    writeToDisk("Tailrec", formatLiftedAst(flix.getTailrecAst))
    writeToDisk("Optimizer", formatLiftedAst(flix.getOptimizerAst))
    writeToDisk("LateTreeShaker", formatLiftedAst(flix.getLateTreeShakerAst))
    // Reducer wip
    // VarNumbering wip
    writeToDisk("MonoTyper", formatMonoTypedAst(flix.getMonoTyperAst))
    writeToDisk("Eraser", formatErasedAst(flix.getEraserAst))
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
  def formatErasedAst(root: ErasedAst.Root): String = {
    formatDocProgram(ErasedAstPrinter.print(root))
  }

  /**
    * Formats `root` for display.
    */
  def formatMonoTypedAst(root: MonoTypedAst.Root): String = {
    formatDocProgram(MonoTypedAstPrinter.print(root))
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
