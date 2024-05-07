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
import ca.uwaterloo.flix.util.{FileOps, InternalCompilerException, Validation}

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  def printParsedAst(phase: String, root: ParsedAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printWeededAst(phase: String, root: WeededAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printDesugaredAst(phase: String, root: DesugaredAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printNamedAst(phase: String, root: NamedAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printResolvedAst(phase: String, root: ResolvedAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printKindedAst(phase: String, root: KindedAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printTypedAst(phase: String, root: TypedAst.Root)(implicit flix: Flix): Unit = {
    printDocProgram(phase, TypedAstPrinter.print(root))
  }

  def printLoweredAst(phase: String, root: LoweredAst.Root)(implicit flix: Flix): Unit = {
    printDocProgram(phase, LoweredAstPrinter.print(root))
  }

  def printMonoAst(phase: String, root: MonoAst.Root)(implicit flix: Flix): Unit = {
    writeToDisk(phase, "Not yet implemented")
  }

  def printSimplifiedAst(phase: String, root: SimplifiedAst.Root)(implicit flix: Flix): Unit = {
    printDocProgram(phase, SimplifiedAstPrinter.print(root))
  }

  def printLiftedAst(phase: String, root: LiftedAst.Root)(implicit flix: Flix): Unit = {
    printDocProgram(phase, LiftedAstPrinter.print(root))
  }

  def printReducedAst(phase: String, root: ReducedAst.Root)(implicit flix: Flix): Unit = {
    printDocProgram(phase, ReducedAstPrinter.print(root))
  }

  def inValidation[A, E, T](f: (String, A) => T): (String, Validation[A, E]) => Unit = {
    (s: String, v: Validation[A, E]) => {
      Validation.mapN(v)(f(s, _))
      ()
    }
  }

  private def printDocProgram(phase: String, dast: DocAst.Program)(implicit flix: Flix): Unit = {
    writeToDisk(phase, formatDocProgram(dast))
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
    * Writes `content` to the file `./build/asts/<phaseName>.flixir`. The build folder is taken from
    * flix options if present. The existing file is overwritten if present.
    */
  private def writeToDisk(phaseName: String, content: String)(implicit flix: Flix): Unit = {
    val filePath = phaseOutputPath(phaseName)
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
  private def phaseOutputPath(phaseName: String)(implicit flix: Flix): Path = {
    astFolderPath.resolve(s"$phaseName.$IrFileExtension")
  }

  /**
    * Returns the path to the folder that holds the pretty printed ast files used by [[writeToDisk]].
    *
    * OBS: this function has no checking so the path might not exist and it might not be readable etc.
    */
  def astFolderPath(implicit flix: Flix): Path = {
    flix.options.output.getOrElse(Path.of("./build/")).resolve("asts/")
  }
}
