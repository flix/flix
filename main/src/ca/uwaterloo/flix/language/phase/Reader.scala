/*
 * Copyright 2017 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
 */

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.{Input, Source}
import ca.uwaterloo.flix.language.ast.{ReadAst, SourceLocation}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.InternalCompilerException

import java.nio.file.Files
import scala.collection.mutable

/**
  * A phase to read inputs into memory.
  */
object Reader {

  /**
    * Reads the given source inputs into memory.
    */
  def run(inputs: List[Input])(implicit flix: Flix): (ReadAst.Root, List[CompilationMessage]) =
    flix.phase("Reader") {

      val result = mutable.Map.empty[Source, Unit]
      for (input <- inputs) {
        input match {
          case Input.RealFile(path, _) =>
            val bytes = Files.readAllBytes(path)
            val str = new String(bytes, flix.defaultCharset)
            val src = Source.fromString(input, str)
            result += (src -> ())

          case Input.VirtualFile(_, text, _) =>
            val src = Source.fromString(input, text)
            result += (src -> ())

          case Input.VirtualUri(_, text, _) =>
            val src = Source.fromString(input, text)
            result += (src -> ())

          case Input.FileInPackage(_, _, text, _) =>
            val src = Source.fromString(input, text)
            result += (src -> ())

          case Input.Unknown => throw InternalCompilerException("Impossible.", SourceLocation.Unknown)
        }
      }

      val sources = result.toMap
      (ReadAst.Root(sources), List.empty)
    }(DebugNoOp())

}
