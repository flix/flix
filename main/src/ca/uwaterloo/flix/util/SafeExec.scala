/*
 * Copyright 2020 Magnus Madsen
 *
 * Use of this source code is governed by the Apache 2.0 license
 * that can be found in the LICENSE.md file.
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

    try {
      // Execute the function f.
      val result = f()

      // Return a triple of the result and the two captured streams as strings.
      val capturedOut = newOut.toString(UTF8)
      val capturedErr = newErr.toString(UTF8)

      (result, capturedOut, capturedErr)
    } finally {
      // Restore the original out and err streams, even if `f` throws.
      System.setOut(originalOut)
      System.setErr(originalErr)
    }
  }

}
