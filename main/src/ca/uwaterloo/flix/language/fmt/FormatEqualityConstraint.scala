/*
 * Copyright 2023 Matthew Lutze
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
package ca.uwaterloo.flix.language.fmt

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.ast.Ast

object FormatEqualityConstraint {

  /** Formats the given `econstr` as `Assoc[Arg] ~ Type`. */
  def formatEqualityConstraint(econstr: Ast.BroadEqualityConstraint)(implicit flix: Flix): String = {
    formatEqualityConstraintWithOptions(econstr, flix.getFormatOptions)
  }

  /** Formats the given `econstr` as `Assoc[Arg] ~ Type`. */
  def formatEqualityConstraintWithOptions(tconstr: Ast.BroadEqualityConstraint, fmt: FormatOptions): String = tconstr match {
    case Ast.BroadEqualityConstraint(tpe1, tpe2) =>
      val tpe1String = FormatType.formatTypeWithOptions(tpe1, fmt)
      val tpe2String = FormatType.formatTypeWithOptions(tpe2, fmt)
      s"${tpe1String} ~ ${tpe2String}"
  }
}
