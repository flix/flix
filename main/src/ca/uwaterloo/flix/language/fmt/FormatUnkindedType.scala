/*
 * Copyright 2022 Matthew Lutze
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

import ca.uwaterloo.flix.language.ast.UnkindedType

object FormatUnkindedType {

  /**
    * Formats the given Unkinded Type.
    *
    * The type must have a real source location.
    */
  def formatUnkindedType(tpe: UnkindedType): String = {
    tpe.loc.text
      .getOrElse("ERR_UNABLE_TO_FORMAT_TYPE")
      .linesIterator
      .mkString(" ")
  }
}
