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
import ca.uwaterloo.flix.language.phase._
import ca.uwaterloo.flix.language.phase.jvm.JvmBackend
import ca.uwaterloo.flix.util.collection.Bimap
import ca.uwaterloo.flix.util.{FileOps, InternalCompilerException}

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /**
    * Print the given pretty printed ast to file if options requests the given
    * phase printed.
    */
  def printPhase(phase: AnyRef, prettyAst: => String)(implicit flix: Flix): Unit = AstPrinter.phaseNames().getBackward(phase) match {
    case Some(phaseName) if flix.options.xprintphase.contains("all") || flix.options.xprintphase.contains(phaseName) =>
      AstPrinter.writeToDisk(phaseName, prettyAst)
    case _ => ()
  }

  def phaseNames(): Bimap[String, AnyRef] = {
    Bimap.empty[String, AnyRef] +
      // frontend
      ("Parser", Parser) +
      ("Weeder", Weeder) +
      ("Desugar", Desugar) +
      ("Namer", Namer) +
      ("Resolver", Resolver) +
      ("Kinder", Kinder) +
      ("Deriver", Deriver) +
      ("Typer", Typer) +
      ("Entrypoint", EntryPoint) +
      ("PredDeps", PredDeps) +
      ("Stratifier", Stratifier) +
      ("PatMatch", PatMatch) +
      ("Redundancy", Redundancy) +
      ("Safety", Safety) +
      // backend
      ("Lowering", Lowering) +
      ("TreeShaker1", TreeShaker1) +
      ("Monomorpher", Monomorpher) +
      ("MonoTypes", MonoTypes) +
      ("Simplifier", Simplifier) +
      ("ClosureConv", ClosureConv) +
      ("LambdaLift", LambdaLift) +
      ("Optimizer", Optimizer) +
      ("TreeShaker2", TreeShaker2) +
      ("EffectBinder", EffectBinder) +
      ("TailPos", TailPos) +
      ("Eraser", Eraser) +
      ("Reducer", Reducer) +
      ("VarOffsets", VarOffsets) +
      ("JvmBackend", JvmBackend)
  }

  /**
    * Formats `root` for display.
    */
  def formatParsedAst(root: ParsedAst.Root): String = "Work in progress"

  /**
    * Formats `root` for display.
    */
  def formatWeededAst(root: WeededAst.Root): String = "Work in progress"

  /**
    * Formats `root` for display.
    */
  def formatDesugaredAst(root: DesugaredAst.Root): String = "Work in progress"

  /**
    * Formats `root` for display.
    */
  def formatNamedAst(root: NamedAst.Root): String = "Work in progress"

  /**
    * Formats `root` for display.
    */
  def formatResolvedAst(root: ResolvedAst.Root): String = "Work in progress"

  /**
    * Formats `root` for display.
    */
  def formatKindedAst(root: KindedAst.Root): String = "Work in progress"

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
  def formatMonoAst(root: MonoAst.Root): String = "Work in progress"

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
  def writeToDisk(fileName: String, content: String)(implicit flix: Flix): Unit = {
    val filePath = phaseOutputPath(fileName)
    Files.createDirectories(filePath.getParent)

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

  /**
    * Returns the path to the pretty printed output of `phaseName` used by [[writeToDisk]].
    *
    * OBS: this function has no checking so the path might not hold the ast and it might not be readable etc.
    */
  def phaseOutputPath(phaseName: String)(implicit flix: Flix): Path = {
    val buildAstsPath = flix.options.output.getOrElse(Path.of("./build/")).resolve("asts/")
    buildAstsPath.resolve(s"$phaseName.$IrFileExtension")
  }
}
