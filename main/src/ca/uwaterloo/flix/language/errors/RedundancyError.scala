/*
 * Copyright 2018 Magnus Madsen
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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.Source
import ca.uwaterloo.flix.language.ast.{Eff, SourceLocation}
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

// TODO: DOC

sealed trait RedundancyError extends CompilationError {
  val kind = "Redundancy Error"
}

object RedundancyError {

  case class DeadCodeError(loc: SourceLocation, msg: String) extends RedundancyError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Error: " + msg << NewLine
      vt << NewLine
      vt << Code(loc, "Look here, you!") << NewLine
    }
  }

  case class UnusedEnumError(loc: SourceLocation) extends RedundancyError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Error: Enum member never used." << NewLine
      vt << NewLine
      vt << Code(loc, "You know you put this here for a reason.") << NewLine
    }
  }

  case class UnreachableCodeError(loc: SourceLocation) extends RedundancyError {
    val source: Source = loc.source
    val message: VirtualTerminal = {
      val vt = new VirtualTerminal()
      vt << Line(kind, source.format) << NewLine
      vt << ">> Error: Code is unreachable." << NewLine
      vt << NewLine
      vt << Code(loc, "You know you put this here for a reason.") << NewLine
    }
  }

}
