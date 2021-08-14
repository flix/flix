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

import java.nio.file.Files

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.Ast.{Input, Source}
import ca.uwaterloo.flix.language.ast.Symbol
import ca.uwaterloo.flix.tools.Packager
import ca.uwaterloo.flix.util.Validation._
import ca.uwaterloo.flix.util.Validation

/**
  * A phase to read inputs into memory.
  */
object Reader extends Phase[List[Input], List[Source]] {

  /**
    * Reads the given source inputs into memory.
    */
  def run(input: List[Input])(implicit flix: Flix): Validation[List[Source], CompilationError] = flix.phase("Reader", this) {
    // Compute the sources.
    val sources = input flatMap {

      /**
        * Internal.
        */
      case Input.Internal(name, text) => Source(name, text.toCharArray) :: Nil

      /**
        * String.
        */
      case Input.Str(text) => Source("???", text.toCharArray) :: Nil

      /**
        * Text file.
        */
      case Input.TxtFile(path) =>
        val bytes = Files.readAllBytes(path)
        Source(path.toString, new String(bytes, flix.defaultCharset).toCharArray) :: Nil

      /**
        * Pkg file.
        */
      case Input.PkgFile(path) => Packager.unpack(path)
    }

    sources.toSuccess
  }


}
