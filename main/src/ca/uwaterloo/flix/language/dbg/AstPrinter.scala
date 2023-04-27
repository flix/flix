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
import ca.uwaterloo.flix.language.ast.{ErasedAst, FinalAst, LiftedAst, SourceLocation}
import ca.uwaterloo.flix.language.dbg.printer.{ErasedAstPrinter, FinalAstPrinter, LiftedAstPrinter}
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  /**
    * Writes all the formatted asts, requested by the flix options, to disk.
    */
  def printAsts()(implicit flix: Flix): Unit = {
    val asts = flix.options.xprintasts
    if (asts.contains("LateTreeShaker") || asts.contains("Pre"))
      writeToDisk("LateTreeShaker", formatLiftedAst(flix.getLateTreeShakerAst))
    if (asts.contains("Undo") || asts.contains("Post"))
      writeToDisk("Undo", formatLiftedAst(flix.getUndoAst))
  }

  /**
    * Writes all the formatted asts to disk.
    */
  def printAllAsts()(implicit flix: Flix): Unit = {
    writeToDisk("LateTreeShaker", formatLiftedAst(flix.getLateTreeShakerAst))
    writeToDisk("Undo", formatLiftedAst(flix.getUndoAst))
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
  def formatFinalAst(root: FinalAst.Root): String = {
    formatDocProgram(FinalAstPrinter.print(root))
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
