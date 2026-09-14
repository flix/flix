/*
 * Copyright 2017 Magnus Madsen
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

package ca.uwaterloo.flix.language.phase

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationMessage
import ca.uwaterloo.flix.language.ast.shared.{Input, Source}
import ca.uwaterloo.flix.language.ast.{ReadAst, SourceLocation}
import ca.uwaterloo.flix.language.dbg.AstPrinter.*
import ca.uwaterloo.flix.util.InternalCompilerException

import scala.collection.mutable

/**
  * A phase to turn inputs into sources.
  *
  * The text of every input is already in memory: it is read when the input is added to the
  * compiler. This phase performs no I/O.
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
          case Input.RealFile(_, text, _) =>
            val src = Source.fromString(input, text)
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
