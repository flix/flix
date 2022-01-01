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
import ca.uwaterloo.flix.language.ast.Ast.{Input, Source}
import ca.uwaterloo.flix.tools.Packager
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

import java.nio.file.Files

/**
  * A phase to read inputs into memory.
  */
object Reader {

  /**
    * Reads the given source inputs into memory.
    */
  def run(inputs: List[Input])(implicit flix: Flix): Validation[List[Source], CompilationMessage] = flix.phase("Reader") {
    // Compute the sources.
    val sources = inputs flatMap {

      /**
        * Internal.
        */
      case input@Input.Text(name, text, stable) => Source(input, text.toCharArray, stable) :: Nil

      /**
        * Text file.
        */
      case input@Input.TxtFile(path) =>
        val bytes = Files.readAllBytes(path)
        val str = new String(bytes, flix.defaultCharset)
        val arr = str.toCharArray
        Source(input, arr, stable = false) :: Nil

      /**
        * Pkg file.
        */
      case i@Input.PkgFile(path) => Packager.unpack(path)
    }

    sources.toSuccess
  }


}
