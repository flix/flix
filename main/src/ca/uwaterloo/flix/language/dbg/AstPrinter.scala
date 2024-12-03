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
import ca.uwaterloo.flix.language.ast.*
import ca.uwaterloo.flix.language.dbg.printer.*
import ca.uwaterloo.flix.util.tc.Debug
import ca.uwaterloo.flix.util.{FileOps, InternalCompilerException, Validation}

import java.nio.file.{Files, LinkOption, Path}

object AstPrinter {

  case class DebugNoOp[T]() extends Debug[T] {
    override val hasAst: Boolean = false
    override def emit(phase: String, root: T)(implicit flix: Flix): Unit = ()
  }

  implicit object DebugUnit extends DebugNoOp[Unit]

  implicit object DebugSyntaxTree extends Debug[SyntaxTree.Root] {
    override def emit(phase: String, root: SyntaxTree.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, SyntaxTreePrinter.print(root))
  }

  implicit object DebugWeededAst extends Debug[WeededAst.Root] {
    override val hasAst: Boolean = false
    override def emit(phase: String, root: WeededAst.Root)(implicit flix: Flix): Unit = ()
  }

  implicit object DebugDesugaredAst extends Debug[DesugaredAst.Root] {
    override val hasAst: Boolean = false
    override def emit(phase: String, root: DesugaredAst.Root)(implicit flix: Flix): Unit = ()
  }

  implicit object DebugNamedAst extends Debug[NamedAst.Root] {
    override val hasAst: Boolean = false
    override def emit(phase: String, root: NamedAst.Root)(implicit flix: Flix): Unit = ()
  }

  implicit object DebugResolvedAst extends Debug[ResolvedAst.Root] {
    override def emit(phase: String, root: ResolvedAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, ResolvedAstPrinter.print(root))
  }

  implicit object DebugKindedAst extends Debug[KindedAst.Root] {
    override val hasAst: Boolean = false
    override def emit(phase: String, root: KindedAst.Root)(implicit flix: Flix): Unit = ()
  }

  implicit object DebugTypedAst extends Debug[TypedAst.Root] {
    override def emit(phase: String, root: TypedAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, TypedAstPrinter.print(root))
  }

  implicit object DebugSimplifiedAst extends Debug[SimplifiedAst.Root] {
    override def emit(phase: String, root: SimplifiedAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, SimplifiedAstPrinter.print(root))
  }

  implicit object DebugLoweredAst extends Debug[LoweredAst.Root] {
    override def emit(phase: String, root: LoweredAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, LoweredAstPrinter.print(root))
  }

  implicit object DebugLiftedAst extends Debug[LiftedAst.Root] {
    override def emit(phase: String, root: LiftedAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, LiftedAstPrinter.print(root))
  }

  implicit object DebugMonoAst extends Debug[MonoAst.Root] {
    override def emit(phase: String, root: MonoAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, MonoAstPrinter.print(root))
  }

  implicit object DebugReducedAst extends Debug[ReducedAst.Root] {
    override def emit(phase: String, root: ReducedAst.Root)(implicit flix: Flix): Unit =
      printDocProgram(phase, ReducedAstPrinter.print(root))
  }

  case class DebugValidation[T, E]()(implicit d: Debug[T]) extends Debug[Validation[T, E]] {
    override val hasAst: Boolean = d.hasAst

    override def output(name: String, v: Validation[T, E])(implicit flix: Flix): Unit =
      Validation.mapN(v) {
        case x => d.output(name, x)
      }

    override def emit(name: String, a: Validation[T, E])(implicit flix: Flix): Unit = ()
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
    FileOps.writeString(filePath, content)
  }

  /** Makes sure that the phases file exists and is empty. */
  def resetPhaseFile()(implicit flix: Flix): Unit = {
    val filePath = astFolderPath.resolve("0phases.txt")
    FileOps.writeString(filePath, "")
  }

  def appendPhaseToDisk(phaseName: String, hasAst: Boolean)(implicit flix: Flix): Unit = {
    val filePath = astFolderPath.resolve("0phases.txt")
    val line = s"$phaseName${if (hasAst) "" else " (no output)"}${System.lineSeparator()}"
    FileOps.writeString(filePath, line, append = true)
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
