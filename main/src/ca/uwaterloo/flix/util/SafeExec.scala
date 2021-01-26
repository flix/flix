/*
 * Copyright 2020 Magnus Madsen
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
package ca.uwaterloo.flix.util

import java.io.{ByteArrayOutputStream, PrintStream}
import java.nio.charset.StandardCharsets

object SafeExec {

  private val UTF8: String = StandardCharsets.UTF_8.name()

  /**
    * Executes the given function `f` capturing its standard output and error output.
    *
    * Returns a triple of its result, standard out, and standard err.
    */
  def execute[T](f: () => T): (T, String, String) = {
    // Capture the original out and err streams.
    val originalOut = System.out
    val originalErr = System.err

    // Construct new buffers for out and err.
    val newOut = new ByteArrayOutputStream()
    val newErr = new ByteArrayOutputStream()

    // Set them.
    System.setOut(new PrintStream(newOut, true, UTF8))
    System.setErr(new PrintStream(newErr, true, UTF8))

    // Execute the function f.
    val result = f()

    // Restore the original out and err streams.
    System.setOut(originalOut)
    System.setErr(originalErr)

    // Return a triple of the result and the two captured streams as strings.
    val capturedOut = newOut.toString(UTF8)
    val capturedErr = newErr.toString(UTF8)

    (result, capturedOut, capturedErr)
  }

}
