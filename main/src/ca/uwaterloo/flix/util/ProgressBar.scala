/*
 * Copyright 2021 Magnus Madsen
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

class ProgressBar {
  /**
    * The characters in the spinner.
    */
  private val SpinnerChars = Array("|", "/", "-", "\\")

  /**
    * An internal counter used to print the spinner.
    */
  private var tick = 0

  def observe(phase: String, o: AnyRef): Unit = print(phase, o.toString)

  /**
    * Prints the given string `msg` from the given `phase` to the terminal.
    *
    * This function flushes the output and should not be called too often.
    */
  private def print(phase: String, msg: String): Unit = synchronized {
    // Compute the next character in the spinner.
    tick = (tick + 1) % SpinnerChars.length
    val spinner = SpinnerChars(tick)

    // Build the string to print and pad it.
    val s = s"$spinner [$phase] $msg"
    val r = s + " " * (80 - s.length)

    // Print the string followed by carriage return.
    System.out.print(s"$r\r")

    // Flush to ensure that the string is printed.
    System.out.flush()
  }

}
