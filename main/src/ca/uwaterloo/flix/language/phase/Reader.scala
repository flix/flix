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

import java.nio.charset.Charset
import java.nio.file.Files
import java.util.zip.ZipFile

import ca.uwaterloo.flix.api.Flix
import ca.uwaterloo.flix.language.CompilationError
import ca.uwaterloo.flix.language.ast.{Ast, Input, Source, Symbol}
import ca.uwaterloo.flix.util.StreamOps
import ca.uwaterloo.flix.util.Validation
import ca.uwaterloo.flix.util.Validation._

/**
  * A phase to read source files into memory.
  */
object Reader extends Phase[(List[Input], Map[Symbol.DefnSym, Ast.Hook]), (List[Source], Map[Symbol.DefnSym, Ast.Hook])] {

  /*
   * Implicitly assumed default charset.
   */
  val DefaultCharset: Charset = Charset.forName("UTF-8")

  /**
    * Reads the given source inputs into memory.
    */
  def run(arg: (List[Input], Map[Symbol.DefnSym, Ast.Hook]))(implicit flix: Flix): Validation[(List[Source], Map[Symbol.DefnSym, Ast.Hook]), CompilationError] = {

    val (input, hooks) = arg

    val inputs = input map {

      /**
        * Internal.
        */
      case Input.Internal(name, text) => Source(name, text.toCharArray)

      /**
        * String.
        */
      case Input.Str(text) => Source("???", text.toCharArray)

      /**
        * Text file.
        */
      case Input.TxtFile(path) =>
        val bytes = Files.readAllBytes(path)
        Source(path.toString, new String(bytes, DefaultCharset).toCharArray)

      /**
        * Zip file.
        */
      case Input.ZipFile(path) =>
        val file = new ZipFile(path.toFile)
        val entry = file.entries().nextElement()
        val inputStream = file.getInputStream(entry)
        val bytes = StreamOps.readAllBytes(inputStream)
        Source(path.toString, new String(bytes, DefaultCharset).toCharArray)
    }

    (inputs, hooks).toSuccess
  }

}
