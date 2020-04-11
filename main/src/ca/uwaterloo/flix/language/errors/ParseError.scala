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

package ca.uwaterloo.flix.language.errors

import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.SourceLocation
import ca.uwaterloo.flix.util.vt.VirtualString._
import ca.uwaterloo.flix.util.vt.VirtualTerminal

/**
  * An error raised to indicate a parse error.
  *
  * @param msg the error message.
  * @param loc the source location.
  */
case class ParseError(msg: String, loc: SourceLocation) extends CompilationError {
  val kind = "Parse Error"
  val message: VirtualTerminal = {
    val vt = new VirtualTerminal
    vt << Line(kind, source.format) << NewLine
    vt << ">> Parse Error:" << NewLine
    vt << NewLine
    vt << Red(msg) << NewLine
  }
}
