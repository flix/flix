/*
 * Copyright 2021 Matthew Lutze
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
import ca.uwaterloo.flix.language.ast.shared.TraitConstraint

object FormatTraitConstraint {

  /**
    * Formats the given `tconstr` as `Class[Param]`.
    */
  def formatTraitConstraint(tconstr: TraitConstraint)(implicit flix: Flix): String = {
    formatTraitConstraintWithOptions(tconstr, flix.getFormatOptions)
  }

  /**
    * Formats the given `tconstr` as `Class[Param]`.
    */
  def formatTraitConstraintWithOptions(tconstr: TraitConstraint, fmt: FormatOptions): String = tconstr match {
    case TraitConstraint(head, arg, _) =>
      val typeString = FormatType.formatTypeWithOptions(arg, fmt)
      s"${head.sym}[${typeString}]"
  }
}
